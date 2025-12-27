package com.gdn.x.product.service.api;

import java.util.List;
import java.util.Map;
import java.util.Set;

import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.vo.AddDeleteVariantRequestVo;
import com.gdn.x.product.model.vo.ItemPickupPointListingUpdateRequestVo;
import com.gdn.x.product.model.vo.ItemPickupPointUpdateRequestVo;
import com.gdn.x.product.rest.web.model.EditItemResponse;
import com.gdn.x.product.rest.web.model.dto.AuditTrailDto;
import com.gdn.x.product.rest.web.model.response.BasicProductAndItemEditedDetailsDTO;

public interface ItemPickupPointSummaryService {


  /**
   * item pickupPoint listing update
   *
   * @param productSku
   * @param productType
   * @param itemPickupPointListingUpdateRequestVo
   * @param updatedItemSet
   * @param itemSkuAndItemMap
   * @param itemPickupPointMap
   * @param newlyAddedPickupPointCodes
   * @param deletedPickupPointCodes
   * @param online
   * @param cncAtL3Level
   * @param editItemResponse
   * @param fbbChangedAtL5
   * @param fbbActivatedPPsFromEditList
   * @param deletedFbbTrueL5
   * @param addDeleteVariantRequestVo
   * @param itemPickupPointUpdateRequestVo
   * @param product
   * @throws Exception
   */
  BasicProductAndItemEditedDetailsDTO updateItemPickupPointListing(String productSku, ProductType productType,
      List<ItemPickupPointListingUpdateRequestVo> itemPickupPointListingUpdateRequestVo, Set<Item> updatedItemSet,
      Map<String, Item> itemSkuAndItemMap, Map<String, ItemPickupPoint> itemPickupPointMap,
      Set<String> newlyAddedPickupPointCodes, Set<String> deletedPickupPointCodes, Boolean online, Boolean cncAtL3Level,
      EditItemResponse editItemResponse, boolean fbbChangedAtL5,
      Set<String> fbbActivatedPPsFromEditList, boolean deletedFbbTrueL5,
      AddDeleteVariantRequestVo addDeleteVariantRequestVo, ItemPickupPointUpdateRequestVo itemPickupPointUpdateRequestVo,
      Product product, boolean isBundleProduct, boolean readFromPrimary) throws Exception;

  /**
   * update external history
   *
   * @param auditTrailDtoList must not be empty
   *
   * @return
   */
  void updateExternalHistoryInPBP(List<AuditTrailDto> auditTrailDtoList);

  /**
   * Set CNC at product level
   *
   * @param productSku
   * @param updatedItemList
   * @param product
   * @return
   */
  boolean setCncActiveAtProductLevel(String productSku, Set<Item> updatedItemList,
    Product product, boolean readFromPrimary);

  /**
   * Update item pick up points
   *
   * @param mandatoryRequestParam
   * @param itemPickupPointUpdateRequestVo
   * @param itemSkuAndItemMap
   */
  EditItemResponse updateItemPickupPoint(MandatoryRequestParam mandatoryRequestParam,
      ItemPickupPointUpdateRequestVo itemPickupPointUpdateRequestVo, Map<String, Item> itemSkuAndItemMap, boolean isBundleProduct,
      boolean readFromPrimary) throws Exception;

  /**
   * item pickupPoint listing update
   *
   * @param productSku
   * @param productType
   * @param itemPickupPointListingUpdateRequestVo
   * @return
   */
  BasicProductAndItemEditedDetailsDTO validateAndUpdateItemPickupPointListing(MandatoryRequestParam mandatoryRequestParam,
      String productSku, ProductType productType, List<ItemPickupPointListingUpdateRequestVo> itemPickupPointListingUpdateRequestVo);
}
