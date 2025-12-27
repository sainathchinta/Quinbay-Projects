package com.gdn.mta.bulk.dto.product;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class UploadProcessCount extends BaseResponse {
  private long count;
}
