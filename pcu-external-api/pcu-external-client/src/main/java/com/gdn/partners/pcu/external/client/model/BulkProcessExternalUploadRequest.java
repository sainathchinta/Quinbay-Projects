package com.gdn.partners.pcu.external.client.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Data;

import java.io.Serializable;
import java.util.Map;

@Data
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkProcessExternalUploadRequest implements Serializable {
    private static final long serialVersionUID = -8470296457105373427L;
    private String zipFileName;
    private Map<String, String> files;
    private String bulkProcessCode;
    private String pickupPointCode;
    private String onlyExternalUser;
    private String businessPartnerCode;
}
