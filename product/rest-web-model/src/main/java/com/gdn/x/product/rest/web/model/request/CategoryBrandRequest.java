package com.gdn.x.product.rest.web.model.request;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class CategoryBrandRequest {

  List<String> brand;
  List<String> category;

  @Override
  public String toString() {
    return "CategoryBrandRequest{" + "brand=" + brand + ", category=" + category + '}';
  }
}
