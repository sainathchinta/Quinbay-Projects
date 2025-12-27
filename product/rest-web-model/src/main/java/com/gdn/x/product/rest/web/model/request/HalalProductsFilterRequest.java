package com.gdn.x.product.rest.web.model.request;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class HalalProductsFilterRequest {
  private String keyword;
  private List<String> categories = new ArrayList<>();
  private List<String> brands = new ArrayList<>();
  private List<String> curationStatus = new ArrayList<>();
}
