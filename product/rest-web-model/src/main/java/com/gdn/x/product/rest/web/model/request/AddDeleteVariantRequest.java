package com.gdn.x.product.rest.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class AddDeleteVariantRequest implements Serializable {
  private static final long serialVersionUID = -7858271024830125605L;
  private List<AddVariantRequest> addVariantsList = new ArrayList<>();
  private List<String> deleteVariantsList = new ArrayList<>();
}