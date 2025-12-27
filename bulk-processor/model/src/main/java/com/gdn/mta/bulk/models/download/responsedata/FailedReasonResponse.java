package com.gdn.mta.bulk.models.download.responsedata;

import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class FailedReasonResponse extends BaseResponse implements Serializable {
  private static final long serialVersionUID = 8418013877312449195L;
  private String itemPickUpPointId;
  private String failedReason;
}