package com.gdn.x.product.rest.web.model;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ActivateNeedRevisionResponse extends BaseResponse {

  private static final long serialVersionUID = 8205174642963107692L;
  boolean createNew;
  List<NewlyAddedL5Response> newlyAddedL5Responses;
}
