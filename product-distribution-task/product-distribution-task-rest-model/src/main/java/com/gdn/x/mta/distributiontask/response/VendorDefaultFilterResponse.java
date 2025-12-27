package com.gdn.x.mta.distributiontask.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties
public class VendorDefaultFilterResponse extends BaseResponse {

    private static final long serialVersionUID = -8401188638416449361L;
    private String vendorEmail;
    private int requestedSkuCount;
    private List<String> assigneeList = new ArrayList<>();
}
