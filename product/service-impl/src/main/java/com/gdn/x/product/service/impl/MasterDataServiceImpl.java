package com.gdn.x.product.service.impl;

import static com.gdn.common.base.GdnPreconditions.checkArgument;

import java.util.Map;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.vo.MasterDataProductAndItemsVO;
import com.gdn.x.product.outbound.api.ProductCategoryBaseOutbound;
import com.gdn.x.product.service.api.MarkForDeleteHelperService;
import com.gdn.x.product.service.api.MasterDataHelperService;
import com.gdn.x.product.service.api.MasterDataService;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;

@Service
public class MasterDataServiceImpl implements MasterDataService {

  private static final Logger LOGGER = LoggerFactory.getLogger(MasterDataServiceImpl.class);

  private static final String PRODUCT_CODE_MUST_NOT_BE_BLANK = "productCode must not be blank";

  private static final String ITEM_CODES_MUST_NOT_BE_NULL_OR_EMPTY =
      "itemCodes must not be null or empty";

  private static final String PRODUCT_NOT_FOUND_WITH_PRODUCT_CODE =
      "product not found with product code : ";

  @Autowired
  private MarkForDeleteHelperService markForDeleteHelperService;

  @Autowired
  private ProductCategoryBaseOutbound productCategoryBaseOutbound;

  @Autowired
  private MasterDataHelperService masterDataHelperService;

  @Override
  public Map<String, MasterDataItem> getMasterDataItems(String storeId, String username,
      String requestId, Set<String> itemCodes) throws Exception {
    return this.masterDataHelperService.getMasterData(MasterDataItem.class, storeId, username,
        requestId, itemCodes, false, true);
  }

  @Override
  public Map<String, MasterDataProductAndItemsVO> getMasterDataProductDetailResponse(
      String storeId, String username, String requestId, Set<String> productCodes, boolean inAllProducts) throws Exception {
    return this.masterDataHelperService.getMasterData(MasterDataProductAndItemsVO.class, storeId,
        username, requestId, productCodes, inAllProducts, true);
  }

  @Override
  public Map<String, MasterDataProductAndItemsVO> getMasterDataProductDetailResponseWithoutCache(
      String storeId, String username, String requestId, Set<String> productCodes, boolean inAllProducts) throws Exception {
    return this.masterDataHelperService.getMasterData(MasterDataProductAndItemsVO.class, storeId,
        username, requestId, productCodes, inAllProducts, false);
  }

  @Override
  public Map<String, MasterDataProduct> getMasterDataProducts(String storeId, String username,
      String requestId, Set<String> productCodesParam) throws Exception {
    return this.masterDataHelperService.getMasterData(MasterDataProduct.class, storeId, username,
        requestId, productCodesParam, false, true);
  }

  @Override
  public ProductDetailResponse getProductDetailFromMasterData(String username, String requestId,
      String productCode) throws Exception {
    checkArgument(StringUtils.isNotBlank(productCode),
        MasterDataServiceImpl.PRODUCT_CODE_MUST_NOT_BE_BLANK);
    ProductDetailResponse productResponse =
        this.productCategoryBaseOutbound.getProductDetailByProductCode(requestId, username,
            productCode);
    checkArgument(productResponse != null,
        MasterDataServiceImpl.PRODUCT_NOT_FOUND_WITH_PRODUCT_CODE + productCode);
    return this.markForDeleteHelperService.removeAllMarkForDelete(productResponse);
  }

}
