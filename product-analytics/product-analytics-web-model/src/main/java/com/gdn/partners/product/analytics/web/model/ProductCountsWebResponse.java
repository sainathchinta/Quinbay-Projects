package com.gdn.partners.product.analytics.web.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@EqualsAndHashCode(callSuper = false)
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductCountsWebResponse extends BaseResponse {
  private static final long serialVersionUID = -4344153862450352467L;
  private int productCount;
}
