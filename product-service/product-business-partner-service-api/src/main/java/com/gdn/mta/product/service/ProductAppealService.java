package com.gdn.mta.product.service;

import com.gda.mta.product.dto.AppealProductConfigResponse;
import com.gda.mta.product.dto.AppealProductRequest;
import com.gda.mta.product.dto.AppealProductResponse;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.product.entity.ProductBusinessPartnerCounter;
import org.apache.commons.lang3.tuple.Pair;

public interface ProductAppealService {

  /**
   * @param storeId Store id
   * @param appealProductRequest AppealProductRequest
   * @return AppealProductResponse
   */
  AppealProductResponse updateAppealProductForInProgressProducts(String storeId,
    AppealProductRequest appealProductRequest) throws ApplicationException;

  /**
   * increment the counter for seller appeal product
   * @param businessPartnerCounter
   */
  void incrementCounterForProductAppeal(ProductBusinessPartnerCounter businessPartnerCounter);

  /**
   * fetch the threshold and counter for seller appeal product
   * @param storeId
   * @param businessPartnerCode
   * @return
   */
  Pair<Integer, ProductBusinessPartnerCounter> fetchThresholdAndCounterForAppealProduct(
      String storeId, String businessPartnerCode);

  /**
   *
   * @param storeId StoreId
   * @param businessPartnerCode Business Partner Code
   */
  void decrementCounterForProductAppeal(String storeId, String businessPartnerCode);


  /**
   * Return threshold count & present limit
   * @param storeId storeId
   * @param businessPartnerCode business partner code
   * @return
   */
  AppealProductConfigResponse fetchAppealProductConfig(String storeId, String businessPartnerCode);
}
