package com.gdn.x.product.service.impl;

import static com.gdn.common.base.GdnPreconditions.checkArgument;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.vo.MasterDataProductAndItemsVO;
import com.gdn.x.product.service.api.MasterDataCacheService;
import com.gdn.x.product.service.api.MasterDataConcurrentService;
import com.gdn.x.product.service.api.MasterDataHelperService;
import com.gdn.x.product.service.util.FormulaUtil;

@Service
public class MasterDataHelperServiceImpl implements MasterDataHelperService {

  private static final Logger LOGGER = LoggerFactory.getLogger(MasterDataHelperServiceImpl.class);

  private static final String PRODUCT_CODES_MUST_NOT_BE_NULL_OR_EMPTY =
      "productCodes must not be null or empty";

  @Autowired
  private MasterDataCacheService masterDataCacheService;

  @Autowired
  private FormulaUtil formulaUtil;

  @Autowired
  private MasterDataConcurrentService masterDataConcurrentService;

  @Override
  @SuppressWarnings("unchecked")
  public <T> Map<String, T> getMasterData(Class<T> masterDataClass, String storeId,
      String username, String requestId, Set<String> codes, boolean inAllProducts, boolean useMasterDataCache) throws Exception {
    checkArgument(codes != null,
        MasterDataHelperServiceImpl.PRODUCT_CODES_MUST_NOT_BE_NULL_OR_EMPTY);
    codes.removeAll(Collections.singleton(null));
    int concurrentSize = this.formulaUtil.getConcurrentSize(storeId, codes);
    if (concurrentSize == 0) {
      return new HashMap<String, T>();
    }
    if (codes.size() == 1) {
      Map<String, T> response = new HashMap<String, T>();
      T masterDataObject;
      String code = codes.iterator().next();
      if (MasterDataItem.class == masterDataClass) {
        MasterDataItem masterDataItem = null;
        if (useMasterDataCache) {
          masterDataItem = this.masterDataCacheService.getMasterDataItem(requestId, username, code);
        } else {
          masterDataItem = this.masterDataCacheService.getMasterDataItemWithoutCache(requestId, username, code);
        }
        masterDataObject = (T) masterDataItem;
      } else {
        MasterDataProductAndItemsVO masterDataProduct = null;
        if (useMasterDataCache) {
          masterDataProduct =
              this.masterDataCacheService.getMasterDataProductAndItems(username, requestId, code, inAllProducts);
        } else {
          masterDataProduct = this.masterDataCacheService
              .getMasterDataProductAndItemsWithoutCache(username, requestId, code, inAllProducts);
        }
        if (MasterDataProduct.class == masterDataClass) {
          masterDataObject = (T) masterDataProduct.getMasterDataProduct();
        } else {
          masterDataObject = (T) masterDataProduct;
        }
      }
      response.put(code, masterDataObject);
      return response;
    } else {
      return this.masterDataConcurrentService.doConcurrentCall(masterDataClass, username,
          requestId, codes, concurrentSize, inAllProducts);
    }
  }

}
