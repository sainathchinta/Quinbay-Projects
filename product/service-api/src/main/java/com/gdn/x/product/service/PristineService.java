package com.gdn.x.product.service;

import com.gdn.common.web.param.MandatoryRequestParam;

import com.gdn.x.product.model.vo.DefaultItemSkuVO;

import com.gdn.x.product.model.vo.PristineItemDetailAndMappingVo;


/**
 * Created by keshashah on 18/12/17.
 */
public interface PristineService {
  /**
   * @param param
   * @param pristineId
   * @param defaultSku When this is null, item is fetched by pristineId
   * @return
   * @throws Exception
   */
  PristineItemDetailAndMappingVo getPristineItemsMappingByPristineId(
      MandatoryRequestParam param, String pristineId, String defaultSku)
      throws Exception;

  /**
   * get default item sku by pristine id
   *
   * @param param
   * @param pristineId
   * @return
   * @throws Exception
   */
  DefaultItemSkuVO getDefaultItemSkuByPristineId(
      MandatoryRequestParam param, String pristineId) throws Exception;

  /**
   * get pristine master id by pristine id, product code or product sku.
   *
   * @param storeId
   * @param username
   * @param requestId
   * @param pristineIdOrProductCodeOrSku
   * @return
   */
  String getPristineMasterIdByPristineIdOrProductCodeOrSku(
      String storeId, String username, String requestId, String pristineIdOrProductCodeOrSku);
}
