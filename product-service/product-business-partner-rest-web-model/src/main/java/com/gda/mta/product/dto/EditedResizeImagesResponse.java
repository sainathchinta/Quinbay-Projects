package com.gda.mta.product.dto;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.micro.graphics.web.model.ImageRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class EditedResizeImagesResponse {
  Map<ProductLevel3SummaryDetailsImageRequest, String> productLevel3SummaryDetailsImageRequestStringMap =
      new HashMap<>();
  List<ImageRequest> editedImages = new ArrayList<>();
}
