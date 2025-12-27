package com.gdn.x.product.outbound.api;

import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.product.model.response.AdjustmentProductChangeResponseVO;
import com.gdn.x.product.model.response.AdjustmentProductResponse;
import com.gdn.x.product.model.vo.ActivePromoBundlingResponseVO;
import com.gdn.x.product.model.vo.PromoBundlingByItemSkuAndItemCodesResponseVO;
import com.gdn.x.product.model.vo.PromoBundlingDetailResponseVO;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest;
import com.gdn.x.promotion.enums.PromoBundlingType;

import java.util.List;
import java.util.Set;

public interface PromotionOutbound {

  /**
   * @param param
   * @param promoBundlingType
   * @param itemSku
   * @param itemCodes
   * @param includeNewBundlings
   * @return
   */
  PromoBundlingByItemSkuAndItemCodesResponseVO getActiveAndPromoBundlingTotalByPromoBundlingType(
      MandatoryRequestParam param, String promoBundlingType, String itemSku, Set<String> itemCodes,
      boolean includeNewBundlings);

  List<AdjustmentProductResponse> getAdjustmentProduct(String requestId, String username,
      List<String> productSkus);

  /**
   * @param param must not be null
   * @param page must not be blank
   * @param size must not be blank
   * @param sortBy must not be blank
   * @param sortType must not be blank
   * @param itemCodes must not be null
   * @return ActivePromoBundlingResponseVO
   */
  ActivePromoBundlingResponseVO getActiveCombosByItemCodes(MandatoryRequestParam param,
      int page, int size, String sortBy, String sortType, Set<String> itemCodes);

  /**
   * @param param must not be null
   * @param promoBundlingType must not be blank
   * @param itemSkus must not be null
   * @return ActivePromoBundlingResponseVO
   */
  ActivePromoBundlingResponseVO getActiveByPromoBundlingTypeAndItemSkus(MandatoryRequestParam param,
      PromoBundlingType promoBundlingType, Set<String> itemSkus);

  /**
   * @param mandatoryRequestParam must not be null
   * @param promoBundlingIds must not be null
   * @param itemSkus
   * @return list of promoBundlingDetailResponse
   */
  List<PromoBundlingDetailResponseVO> getPromoBundlingDetailByPromoBundlingIds(MandatoryRequestParam mandatoryRequestParam,
      Set<String> promoBundlingIds, Set<String> itemSkus);

  /**
   * Fetch Adjustment details of active campaign from promotion with Item SKU and PP Code
   * @param itemRequests must not be null
   * @return list of adjustmentProductChangeResponse
   */
  List<AdjustmentProductChangeResponseVO> getAdjustmentProductBySkuAndPickupPointCode(String storeId,
    List<ItemPickupPointRequest> itemRequests);
}
