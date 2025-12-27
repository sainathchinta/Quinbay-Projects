package com.gdn.x.product.outbound.api;

import java.util.List;
import java.util.Set;

import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.partners.product.pricing.web.model.promo.bundling.request.ItemInfoRequest;
import com.gdn.partners.product.pricing.web.model.promo.bundling.response.PromoBundlingSkuDetailResponse;
import com.gdn.x.product.model.vo.ActivePromoBundlingResponseVO;
import com.gdn.x.product.model.vo.PromoBundlingByItemSkuAndItemCodesResponseVO;
import com.gdn.x.product.model.vo.PromoBundlingDetailResponseVO;

/**
 * @author nitinmathew - created on 03/02/2020
 **/
public interface ProductPricingOutbound {

  /**
   * Get active promo bundling by type and item skus
   *
   * @param param
   * @param promoBundlingType
   * @param itemSkus
   * @return
   * @throws Exception
   */
  ActivePromoBundlingResponseVO findActiveByPromoBundlingTypeAndItemSkus(MandatoryRequestParam param,
      String promoBundlingType, Set<String> itemSkus) throws Exception;

  /**
   * Get active promo bundling by type and item skus and pickup point code
   * @param param
   * @param promoBundlingType
   * @param itemInfoRequests
   * @return
   */
  List<PromoBundlingSkuDetailResponse> findActiveByPromoBundlingTypeAndItemSkusAndPickupPointCode(MandatoryRequestParam param,
      String promoBundlingType, Set<ItemInfoRequest> itemInfoRequests);

  /**
   * Get active promo bundling by type and item codes
   *
   * @param param
   * @param page
   * @param size
   * @param sortBy
   * @param sortType
   * @param promoBundlingType
   * @param itemCodes
   * @return
   */
  ActivePromoBundlingResponseVO findActiveByPromoBundlingTypeAndItemCodes(MandatoryRequestParam param, int page,
      int size, String sortBy, String sortType, String promoBundlingType, Set<String> itemCodes) throws Exception;

  /**
   * Get active promo bundlings from promo bundling Ids
   *
   * @param param
   * @param promoBundlingIds
   * @param itemSkus
   * @return
   */
  List<PromoBundlingDetailResponseVO> findActivePromoBundlingByIds(MandatoryRequestParam param,
      Set<String> promoBundlingIds, Set<String> itemSkus) throws Exception;

  /**
   * Get active promo bundling total by promo bundling type, item sku and item codes
   *
   * @param param
   * @param promoBundlingType
   * @param itemSku
   * @param itemCodes
   * @param includeNewBundlings
   * @return
   */
  PromoBundlingByItemSkuAndItemCodesResponseVO getActiveAndPromoBundlingTotalByPromoBundlingType(
      MandatoryRequestParam param, String promoBundlingType, String itemSku, Set<String> itemCodes,
      boolean includeNewBundlings) throws Exception;
}
