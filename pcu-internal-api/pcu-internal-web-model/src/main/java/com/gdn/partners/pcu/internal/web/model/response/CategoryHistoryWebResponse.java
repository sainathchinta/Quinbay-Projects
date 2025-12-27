package com.gdn.partners.pcu.internal.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.productcategorybase.entity.GdnBaseEntity;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@EqualsAndHashCode(callSuper = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryHistoryWebResponse extends BaseResponse {
  private String activity;
  private String currentValue;
  private String previousValue;
  private String categoryCode;
}
