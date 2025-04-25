class UrlUtils {
    public sanitiseUrl(url: string) {
        if (url.includes("?")) {
            url = url.substring(0, url.indexOf("?"));
        }

        return url
            .trim()
            .split("/")
            .map(segment => {
                const regexForOnlyNumericId = /^[0-9]+$/;
                const regexForOnlyUuid = /^.*[a-z][0-9].*-.*[a-z][0-9].*-.*$/i;

                if (segment.match(regexForOnlyNumericId) || segment.match(regexForOnlyUuid)) {
                    return "{ID}";
                }

                return segment;
            })
            .join("/")
            .replace(/[^a-zA-Z0-9{} ]/g, '-');
    }
}

export default new UrlUtils();
