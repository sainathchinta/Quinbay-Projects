package com.gdn.x.mta.distributiontask.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class VendorDefaultFilterRequest implements Serializable {

    private static final long serialVersionUID = 900256187840874682L;
    private List<String> assigneeList = new ArrayList<>();
    private String vendorEmail;
    private int requestedSkuCount;
    private boolean defaultSettingsEnabled;
}
