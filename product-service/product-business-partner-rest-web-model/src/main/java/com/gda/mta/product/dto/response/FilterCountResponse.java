package com.gda.mta.product.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class FilterCountResponse extends BaseResponse {

    private int yesterday;
    private int threeToFiveDays;
    private int twoDaysAgo;
    private int today;
    private int moreThanFiveDaysAgo;
    private int assigned;
    private int unassigned;
    private int revised;
    private int brandApproved;
    private int brandNotApproved;
    private boolean isSourceDb;
}
