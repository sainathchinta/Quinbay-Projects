package com.gda.mta.product.dto;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.micro.graphics.web.model.ImageRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemImageUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.ProductItemImageRequest;
import com.google.common.collect.ImmutableMap;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class EditedResizeAndImagesUpdateStatusResponse {
  Map<String, Boolean> itemImagesUpdateStatus = new HashMap<>();
  List<ImageRequest> editedImages = new ArrayList<>();
  List<ProductLevel3SummaryDetailsImageRequest> productLevel3SummaryDetailsImageRequests = new ArrayList<>();
  ProductItemImageHistoryDTO productItemImageHistoryDTO;
}
