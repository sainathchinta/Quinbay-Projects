package com.gdn.mta.bulk.models.download.responsedata;

import java.io.Serial;
import java.io.Serializable;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.wrapper.response.PageMetaData;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkDownloadProductLevel4Response implements Serializable {
    @Serial
    private static final long serialVersionUID = 3232423L;
    private List<BulkProductEANResponse> bulkProductEANResponse;
    private Map<String, String> exceptionMap = new HashMap();
    private PageMetaData pageMetaData = new PageMetaData();

    public BulkDownloadProductLevel4Response(List<BulkProductEANResponse> bulkProductEANResponses) {
        this.bulkProductEANResponse = bulkProductEANResponses;
    }
}
