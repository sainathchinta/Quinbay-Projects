package com.gdn.partners.pcu.external.web.model.request;


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
public class ProductImageEditWebRequest {
  String imageData;
  String imagePath;
  List<ItemImageEditWebRequest> productItems = new ArrayList<>();
  CopyImageEditWebRequest copyToAllVariantImages;
}
