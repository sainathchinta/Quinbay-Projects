package com.gdn.mta.bulk.models.download.responsedata;

import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class BulkRebateResponse extends BaseResponse {

  private static final long serialVersionUID = 6168112064636746090L;
  private String failedReason;
}
