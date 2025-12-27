package com.gdn.x.product.service.task;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.vo.MasterDataProductAndItemsVO;
import com.gdn.x.product.service.api.MasterDataCacheService;

public class GetMasterDataTask<T> implements Callable<Map<String, T>> {

  private static final Logger LOGGER = LoggerFactory.getLogger(GetMasterDataTask.class);

  private Class<T> masterDataClass;
  private List<String> codes;
  private String requestId;
  private String username;
  private MasterDataCacheService masterDataCacheService;
  private boolean inAllProducts;

  public GetMasterDataTask() {
    // do nothing
  }

  public GetMasterDataTask(Class<T> masterDataClass, List<String> codes, String requestId,
      String username, MasterDataCacheService masterDataCacheService) {
    super();
    this.masterDataClass = masterDataClass;
    this.codes = codes;
    this.requestId = requestId;
    this.username = username;
    this.masterDataCacheService = masterDataCacheService;
  }

  public GetMasterDataTask(Class<T> masterDataClass, List<String> codes, String requestId,
      String username, MasterDataCacheService masterDataCacheService, boolean inAllProducts) {
    super();
    this.masterDataClass = masterDataClass;
    this.codes = codes;
    this.requestId = requestId;
    this.username = username;
    this.masterDataCacheService = masterDataCacheService;
    this.inAllProducts = inAllProducts;
  }

  @SuppressWarnings("unchecked")
  @Override
  public Map<String, T> call() {
    Map<String, T> mapOfResult = new HashMap<String, T>();
    if (this.masterDataClass == MasterDataItem.class) {
      for (String itemCode : this.codes) {
        try {
          mapOfResult.put(itemCode, (T) this.masterDataCacheService
              .getMasterDataItem(this.requestId, this.username, itemCode));
        } catch (Exception e) {
          GetMasterDataTask.LOGGER.error("#getMasterDataItemFailure for itemCode:{}, with error:",
              itemCode, e.getMessage());
        }
      }
    } else if (this.masterDataClass == MasterDataProduct.class) {
      for (String productCode : this.codes) {
        try {
          mapOfResult.put(productCode,
              (T) this.masterDataCacheService
                  .getMasterDataProductAndItems(this.requestId, this.username, productCode, this.inAllProducts)
                  .getMasterDataProduct());
        } catch (Exception e) {
          GetMasterDataTask.LOGGER.error(
              "#getMasterDataProductFailure for productCode:{}, with error: {}", productCode,
              e.getMessage(), e);
        }
      }
    } else if (this.masterDataClass == MasterDataProductAndItemsVO.class) {
      for (String productCode : this.codes) {
        try {
          mapOfResult.put(productCode, (T) this.masterDataCacheService
              .getMasterDataProductAndItems(this.username, this.requestId, productCode, this.inAllProducts));
        } catch (Exception e) {
          GetMasterDataTask.LOGGER.error(
              "#getMasterDataProductDetailFailure for productCode : {}, with error : {}",
              productCode, e.getMessage(), e);
        }
      }
    }
    return mapOfResult;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }


  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }


}
