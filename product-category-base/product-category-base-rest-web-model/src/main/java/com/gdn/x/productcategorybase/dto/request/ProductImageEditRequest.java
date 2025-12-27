package com.gdn.x.productcategorybase.dto.request;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTORequest;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductImageEditRequest extends BaseDTORequest {

  private static final long serialVersionUID = -8611389870882810163L;

  private String productCode;
  private String imagePath;
  private boolean needRevision;
  private boolean activatedBefore;
  private List<ItemImageEditRequest> productItems = new ArrayList<>();
  private CopyImageEditRequest copyToAllVariantImages;
}
