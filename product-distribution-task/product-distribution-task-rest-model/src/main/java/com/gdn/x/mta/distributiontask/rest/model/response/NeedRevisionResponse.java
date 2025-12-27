package com.gdn.x.mta.distributiontask.rest.model.response;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.mta.distributiontask.response.VendorDetailResponse;
import com.gdn.x.mta.distributiontask.rest.model.constant.WorkflowWebState;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Created by Poornima on 9/19/16.
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class NeedRevisionResponse extends BaseResponse {

  private static final long serialVersionUID = 2093247139839836625L;
  boolean contentNeedCorrection;
  boolean imageNeedCorrection;
  boolean success;
  int failedToUpdateNeedCorrectionProductsCount;

}