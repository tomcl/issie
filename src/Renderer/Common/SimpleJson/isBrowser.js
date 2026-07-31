export default function isBrowser() {
    return typeof window !== "undefined" && typeof window.document !== "undefined";
}