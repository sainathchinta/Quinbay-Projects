package com.gdn.x.product.rest.web.model.response;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.rest.web.model.dto.MasterCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductDTO;
import com.gdn.x.product.rest.web.model.dto.PreOrderDTO;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.ProductSpecialAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.SalesCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.SalesCategorySequenceDTO;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class PrdProductResponse extends BaseResponse {
  private static final long serialVersionUID = -462885970974434508L;

  private String productSku;

  private String productCode;

  private ProductType productType;

  private String merchantCode;

  private boolean isSynchronized;

  private boolean isSuspended;

  private String productCatentryId;

  private List<ProductAttributeDTO> definingAttributes = new ArrayList<ProductAttributeDTO>();

  private List<ProductSpecialAttributeDTO> productSpecialAttributes;

  private MasterCatalogDTO masterCatalog;

  private List<SalesCatalogDTO> salesCatalogs;

  private MasterDataProductDTO masterDataProduct;

  private List<SalesCategorySequenceDTO> salesCategorySequences;

  private boolean installationRequired;

  private int off2OnItemCount;

  private boolean tradingProduct;

  private boolean forceReview;

  private ProductScoreResponse productScore;

  private Date productCenterUpdatedDate;

  private boolean isArchived;

  private boolean isArchivedBeforeSuspension;

  private boolean off2OnChannelActive;

  private PreOrderDTO preOrder;

  private boolean takenDown;

  private boolean freeSample;

  private String productName;

  private String brand;

  private boolean cncActivated;

  private Set<String> pickupPointCodes = new HashSet<String>();

  private boolean online;

  private String categoryCode;

  private boolean markForDelete;

  private String storeId;
}
