package com.gda.mta.product.dto;

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
public class ProductImageEditRequest extends BaseRequest {

  private static final long serialVersionUID = -8611389870882810163L;

  private String productCode;
  private String productSku;
  private String businessPartnerCode;
  private String imagePath;
  private String imageData;
  private String hashCode;
  private List<ItemImageEditRequest> productItems = new ArrayList<>();
  private CopyImageEditRequest copyToAllVariantImages;
  private boolean imageAdded = false;
  private boolean imageUpdated = false;
}
