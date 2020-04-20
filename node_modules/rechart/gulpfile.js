var assign = require('lodash').assign
var browserify = require('browserify');
var coffee_reactify = require('coffee-reactify');
var cjsx = require('gulp-cjsx');
var coffee = require('gulp-coffee');
var exorcist = require('exorcist');
var gulp = require('gulp');
var gutil = require('gulp-util');
var notify = require('gulp-notify');
var path = require('path');
var rename = require('gulp-rename');
var source = require('vinyl-source-stream');
var transform = require('vinyl-transform');
var watchify = require('watchify');

var src = 'examples/';
var dest = 'examples/';

var buildScriptDev = function (watch) {
    var rebundle = function () {
        var stream = bundler.bundle();
        return stream
            .on('error', notify.onError({
                title: 'Compile Error',
                message: '<%= error.message %>'
            }))
            .pipe(source('index.dev.js'))
            .pipe(transform(function (full_path) {
                var filename = path.basename(full_path);
                return exorcist(dest + filename + '.map');
            }))
            .pipe(gulp.dest(dest));
    };

    var opts = assign({}, watchify.args, {
        extensions: ['.coffee', '.cjsx'],
        ignoreMissing: true,
        debug: true
    });
    var bundler = browserify(src + 'index.cjsx', opts);
    if (watch) {
        bundler = watchify(bundler);
    }
    bundler.transform(coffee_reactify);
    bundler.on('update', function (files) {
        gutil.log('Detected change in', files.join(', '));
        gutil.log('Rebuilding...');
        rebundle();
        gutil.log('DONE');
        return
    });

    rebundle();
};


var handleError = function(e) {
    gutil.log(e);
    this.emit('end');
};

gulp.task('dev_js', [], function() {
    buildScriptDev(false);
});

gulp.task('build', [], function() {
    gulp.src('./src/**/*.cjsx')
        .pipe(cjsx({bare: true}).on('error', gutil.log))
        .pipe(gulp.dest('./lib/'));

    gulp.src('./src/**/*.coffee')
        .pipe(coffee({bare: true}).on('error', gutil.log))
        .pipe(gulp.dest('./lib/'));

    gulp.src('./src/**/*.js').pipe(gulp.dest('./lib/'));
});

gulp.task('watch', ['dev_js'], function() {
    buildScriptDev(true);
});


gulp.task('default', [], function() {
    return gulp.start('watch');
});

