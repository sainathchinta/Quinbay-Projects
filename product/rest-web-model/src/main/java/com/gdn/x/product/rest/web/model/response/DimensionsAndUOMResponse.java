package com.gdn.x.product.rest.web.model.response;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class DimensionsAndUOMResponse {

  private String uomCode;
  private String uomType;
  private String uomName;
  private double length;
  private double width;
  private double height;
  private double weight;
  private double conversion;
  private List<String> upcEanList = new ArrayList<>();
}