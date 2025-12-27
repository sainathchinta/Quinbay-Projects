package com.gdn.x.productcategorybase.dto.response;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;
import com.gdn.x.productcategorybase.dto.Image;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class BasicInfoProductResponse extends BaseDTOResponse {
  private String productCode;
  private String productName;
  private String brand;
  private String description;
  private List<Image> commonImageList = new ArrayList<>();
  private Double length;
  private Double width;
  private Double height;
  private Double weight;
  private Double shippingWeight;
  private String video;
}
