package com.gdn.mta.bulk.request;


import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class BulkBasicInfoUpdateRequest {
  private String productSku;
  private String productName;
  private boolean inStore;
  private String brand;
  private String category;
  private String description;
  private List<String> commonImages = new ArrayList<>();
  private String videoUrl;
  private String shippingType;
  private Double length;
  private Double width;
  private Double height;
  private Double weight;
  private Double shippingWeight;
  private String sizeChartCode;
  private boolean youtubeUrl;
  private String mainImageUrl;
  private boolean imagesUpdated;
}