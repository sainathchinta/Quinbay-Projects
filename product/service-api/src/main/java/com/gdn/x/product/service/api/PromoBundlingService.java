package com.gdn.x.product.service.api;

import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.vo.ActiveComboRequestVO;
import com.gdn.x.product.model.vo.ComboDetailVo;
import com.gdn.x.product.model.vo.ComboResponseVO;
import com.gdn.x.product.model.vo.PromoBundlingDetailResponseVO;
import com.gdn.x.product.model.vo.WholesaleRuleVO;
import com.gdn.x.product.model.vo.WholesaleVO;
import com.gdn.common.web.param.MandatoryRequestParam;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Created by w.william on 3/9/2018.
 */
public interface PromoBundlingService {

  /**
   * @param storeId must not be blank
   * @param channelId must not be blank
   * @param clientId must not be blank
   * @param requestId must not be blank
   * @param activeComboRequestVO must not be null
   * @param page must not be blank
   * @param size must not be blank
   * @param sortBy must not be blank
   * @param sortType must not be blank
   * @return list of ComboVO
   * @throws Exception
   */
  ComboResponseVO getActiveCombos(String storeId, String channelId,
      String clientId, String requestId, String username, ActiveComboRequestVO activeComboRequestVO, int page,
      int size, String sortBy, String sortType) throws Exception;

  /**
   * Get promo detail by itemSku
   *
   * @param storeId
   * @param channelId
   * @param clientId
   * @param requestId
   * @param username
   * @param itemSku
   * @param isPristine
   * @param includeNewBundlings
   * @return
   * @throws Exception
   */
  ComboDetailVo getComboDetailByItemSku(String storeId, String channelId, String clientId, String requestId,
      String username, String itemSku, boolean isPristine, boolean includeNewBundlings) throws Exception;

  /**
   * @param storeId must not be blank
   * @param channelId must not be blank
   * @param clientId must not be blank
   * @param requestId must not be blank
   * @param itemSku must not be blank
   * @param isPristine must not be null
   * @param includeNewBundlings
   * @return wholesale VO
   * @throws Exception
   */
  WholesaleVO getWholesaleByItemSku(String storeId, String channelId, String clientId, String requestId,
      String username, String itemSku, boolean isPristine, boolean includeNewBundlings) throws Exception;

  /**
   * @param param must not be null
   * @param promoBundlingIds must not be null
   * @param itemSkus
   * @return List of promoBundlingDetailResponseVO
   */
  List<PromoBundlingDetailResponseVO> getByPromoBundlingIds(MandatoryRequestParam param, Set<String> promoBundlingIds,
      Set<String> itemSkus);

  /**
   * set wholesale rules
   * @param mandatoryRequestParam
   * @param itemPickupPointList
   * @return
   */
  Map<String, List<WholesaleRuleVO>> getWholesaleRules(MandatoryRequestParam mandatoryRequestParam, List<ItemPickupPoint> itemPickupPointList);
}
