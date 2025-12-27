package com.gdn.x.mta.distributiontask.service.api;

import com.gdn.x.mta.distributiontask.model.AppealedProduct;

public interface AppealProductService {

  /**
   * find appeal product by product code
   *
   * @param productCode productCode
   * @return AppealProduct
   */
  AppealedProduct findAppealProductByProductCode(String productCode);

  /**
   * to upsert appeal product
   *
   * @param appealedProduct appealProduct
   */
  void upsertAppealProduct(AppealedProduct appealedProduct);
}
