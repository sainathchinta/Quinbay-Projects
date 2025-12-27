package com.gdn.x.product.rest.web.model.response;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;

import lombok.Data;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Data
public class BundleRecipeResponse implements Serializable {
  private static final long serialVersionUID = 6433732380098084751L;
  List<BundleItemResponse> bundleItemResponses;
}
