namespace Elmish.HMR

open Fable.Core.JsInterop
open Fable.React
open Browser
open Elmish

[<AutoOpen>]
module Common =

    [<NoComparison; NoEquality>]
    type LazyProps<'model> = {
        model: 'model
        render: unit -> ReactElement
        equal: 'model -> 'model -> bool
    }

    type LazyState =
        { HMRCount : int }

    module Components =
        type LazyView<'model>(props) =
            inherit Component<LazyProps<'model>,LazyState>(props)

            let hmrCount =
                if isNull window?Elmish_HMR_Count then
                    0
                else
                    unbox<int> window?Elmish_HMR_Count

            do base.setInitState({ HMRCount = hmrCount})

            override this.shouldComponentUpdate(nextProps, _nextState) =
                // Note: It seems like if the tabs is not focus
                // It can happen that the re-render doesn't happen
                // I am not sure why
                // In theory, this should not be a problem most of the times
                match Bundler.current with
                | Some _ ->
                    let currentHmrCount : int = window?Elmish_HMR_Count
                    if currentHmrCount > this.state.HMRCount then
                        this.setState(fun _prevState _props ->
                            { HMRCount = currentHmrCount }
                        )
                        // An HMR call has been triggered between two frames we force a rendering
                        true
                    else
                        not <| this.props.equal this.props.model nextProps.model

                | None ->
                    not <| this.props.equal this.props.model nextProps.model

            override this.render () =
                this.props.render ()

    #if DEBUG
    /// Avoid rendering the view unless the model has changed.
    /// equal: function to compare the previous and the new states
    /// view: function to render the model
    /// state: new state to render
    let lazyViewWith (equal:'model->'model->bool)
                     (view:'model->ReactElement)
                     (state:'model) =
        ofType<Components.LazyView<_>,_,_>
            { render = fun () -> view state
              equal = equal
              model = state }
            []
    #else
    /// Avoid rendering the view unless the model has changed.
    /// equal: function to compare the previous and the new states
    /// view: function to render the model
    /// state: new state to render
    let inline lazyViewWith (equal:'model->'model->bool)
                     (view:'model->ReactElement)
                     (state:'model) =
        Elmish.React.Common.lazyViewWith equal view state
    #endif

    #if DEBUG
    /// Avoid rendering the view unless the model has changed.
    /// equal: function to compare the previous and the new states
    /// view: function to render the model using the dispatch
    /// state: new state to render
    /// dispatch: dispatch function
    let lazyView2With (equal:'model->'model->bool)
                      (view:'model->'msg Dispatch->ReactElement)
                      (state:'model)
                      (dispatch:'msg Dispatch) =
        ofType<Components.LazyView<_>,_,_>
            { render = fun () -> view state dispatch
              equal = equal
              model = state }
            []
    #else
    /// Avoid rendering the view unless the model has changed.
    /// equal: function to compare the previous and the new states
    /// view: function to render the model using the dispatch
    /// state: new state to render
    /// dispatch: dispatch function
    let inline lazyView2With (equal:'model->'model->bool)
                      (view:'model->'msg Dispatch->ReactElement)
                      (state:'model)
                      (dispatch:'msg Dispatch) =
        Elmish.React.Common.lazyView2With equal view state dispatch
    #endif

    #if DEBUG
    /// Avoid rendering the view unless the model has changed.
    /// equal: function to compare the previous and the new model (a tuple of two states)
    /// view: function to render the model using the dispatch
    /// state1: new state to render
    /// state2: new state to render
    /// dispatch: dispatch function
    let lazyView3With (equal:_->_->bool) (view:_->_->_->ReactElement) state1 state2 (dispatch:'msg Dispatch) =
        ofType<Components.LazyView<_>,_,_>
            { render = fun () -> view state1 state2 dispatch
              equal = equal
              model = (state1,state2) }
            []
    #else
    /// Avoid rendering the view unless the model has changed.
    /// equal: function to compare the previous and the new model (a tuple of two states)
    /// view: function to render the model using the dispatch
    /// state1: new state to render
    /// state2: new state to render
    /// dispatch: dispatch function
    let inline lazyView3With (equal:_->_->bool) (view:_->_->_->ReactElement) state1 state2 (dispatch:'msg Dispatch) =
        Elmish.React.Common.lazyView3With equal view state1 state2 dispatch
    #endif


    #if DEBUG
    /// Avoid rendering the view unless the model has changed.
    /// view: function of model to render the view
    let inline lazyView (view:'model->ReactElement) =
        lazyViewWith (=) view
    #else
    /// Avoid rendering the view unless the model has changed.
    /// view: function of model to render the view
    let inline lazyView (view:'model->ReactElement) =
        Elmish.React.Common.lazyView view
    #endif

    #if DEBUG
    /// Avoid rendering the view unless the model has changed.
    /// view: function of two arguments to render the model using the dispatch
    let lazyView2 (view:'model->'msg Dispatch->ReactElement) =
        lazyView2With (=) view
    #else
    /// Avoid rendering the view unless the model has changed.
    /// view: function of two arguments to render the model using the dispatch
    let inline lazyView2 (view:'model->'msg Dispatch->ReactElement) =
        Elmish.React.Common.lazyView2 view
    #endif

    #if DEBUG
    /// Avoid rendering the view unless the model has changed.
    /// view: function of three arguments to render the model using the dispatch
    let lazyView3 (view:_->_->_->ReactElement) =
        lazyView3With (=) view
    #else
    /// Avoid rendering the view unless the model has changed.
    /// view: function of three arguments to render the model using the dispatch
    let inline lazyView3 (view:_->_->_->ReactElement) =
        Elmish.React.Common.lazyView3 view
    #endif
