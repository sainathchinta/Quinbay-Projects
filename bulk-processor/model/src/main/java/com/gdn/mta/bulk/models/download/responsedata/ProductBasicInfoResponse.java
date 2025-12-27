package com.gdn.mta.bulk.models.download.responsedata;

import java.util.ArrayList;
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
public class ProductBasicInfoResponse  extends BaseResponse {
  private String productSku;
  private String productName;
  private Integer instore;
  private String brand;
  private String categoryCode;
  private String description;
  private List<ImageBasicInfoResponse> commonImageList = new ArrayList<>();
  private String mainImage;
  private String commonImage2;
  private String commonImage3;
  private String commonImage4;
  private String commonImage5;
  private String commonImage6;
  private String commonImage7;
  private String commonImage8;
  private String videoUrl;
  private String shippingType;
  private Double length;
  private Double width;
  private Double height;
  private Double weight;
  private Double shippingWeight;
  private String sizeChartCode;
  private Integer logisticAdjustment;
}
