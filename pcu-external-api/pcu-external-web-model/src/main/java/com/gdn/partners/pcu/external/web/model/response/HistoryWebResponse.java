package com.gdn.partners.pcu.external.web.model.response;

import java.util.Date;

import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
@Builder
public class HistoryWebResponse extends BaseResponse {
  private String activity;
  private String notes;
}
