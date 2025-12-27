package com.gdn.x.mta.distributiontask.rest.model.response;

import java.util.List;

import com.gdn.common.web.base.BaseResponse;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@Builder
@AllArgsConstructor
public class VendorQuickApprovalResponse extends BaseResponse {
  private List<String> errorCodes;
}
