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
public class ProductVariantUpdateRequest extends BaseRequest {

  private static final long serialVersionUID = -3594765958936771851L;
  private List<ProductVariantPriceStockAndImagesRequest> productItems = new ArrayList<>();
  private boolean productEditable;
  private boolean synchronize;
  private boolean needCorrection;
  private String productSku;
  private String businessPartnerCode;
  private String accessChannel;
  private String productCode;
  private String productName;
  private boolean freeSample;
  private Boolean online;
  private Boolean cnc;
  private Boolean fbbActiveAtL3Level;
  private Boolean b2bActivated;
  private Boolean b2cActivated;
  private List<DeletedProductItems> deletedProductItems = new ArrayList<>();
  private List<ProductLevel3SummaryDetailsImageRequest> copyToAllVariantImages = new ArrayList<>();
  private List<ItemPickupPointRequest> addPickupPoints = new ArrayList<>();
  private List<PickupPointDeleteRequest> deletePickupPoints = new ArrayList<>();
  private List<ProductBundleRecipeRequest> productBundleRecipe = new ArrayList<>();
  private PreOrderRequest preOrder;
  private boolean onlyL5Update;
}