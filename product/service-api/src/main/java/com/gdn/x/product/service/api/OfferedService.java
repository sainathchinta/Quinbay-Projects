package com.gdn.x.product.service.api;

import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.product.model.vo.OfferedSummaryVo;

/**
 * Created by w.william on 2/26/2018.
 */
public interface OfferedService {

  OfferedSummaryVo getOfferedSummaryByItemCode(String storeId,
      String username, String requestId, String itemCode, String defaultSku);


  /**
   * @param param must not be null
   * @param pristineId must not be blank
   * @param itemCode must not be blank
   * @param itemSku must not be blank
   * @param defaultSku must not be blank
   * @return
   */
  OfferedSummaryVo getOfferedComboSummary(MandatoryRequestParam param,
      String pristineId, String itemCode, String itemSku, String defaultSku) throws Exception;

  OfferedSummaryVo getOfferedSummary(MandatoryRequestParam param,
      String pristineId, String itemCode, String itemSku, String defaultSku) throws Exception;
}
