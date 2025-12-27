package com.gdn.x.productcategorybase.dto.response;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class DimensionsAndUomResponse implements Serializable {
  private String uomCode;
  private String uomType;
  private String uomName;
  private double length;
  private double width;
  private double height;
  private double weight;
  private double conversion;
  private List<String> upcEanList = new ArrayList<>();
  private List<String> upcEan = new ArrayList<>();

  public DimensionsAndUomResponse(String uomCode, String uomType, String uomName, double length, double width,
      double height, double weight, double conversion, List<String> upcEanList) {
    this.uomCode = uomCode;
    this.uomType = uomType;
    this.uomName = uomName;
    this.length = length;
    this.width = width;
    this.height = height;
    this.weight = weight;
    this.conversion = conversion;
    this.upcEanList = upcEanList;
  }
}
