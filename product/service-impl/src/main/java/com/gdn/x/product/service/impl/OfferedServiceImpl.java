package com.gdn.x.product.service.impl;

import static com.gdn.common.base.GdnPreconditions.checkArgument;
import static com.gdn.common.helper.GdnCommonHelper.denullify;
import static com.gdn.common.helper.GdnCommonHelper.getIfNotNull;

import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.vo.MasterDataProductAndItemsVO;
import com.gdn.x.product.model.vo.OfferedSummaryVo;
import com.gdn.x.product.service.PristineService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.MasterDataService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.OfferedService;
import com.gdn.x.product.service.api.ProductService;

/**
 * Created by w.william on 2/26/2018.
 */
@Service
public class OfferedServiceImpl implements OfferedService {

  private static final Logger LOG = LoggerFactory.getLogger(OfferedServiceImpl.class);

  private static final String STORE_ID_MUST_NOT_BE_BLANK = "storeId must not be blank";
  private static final String ITEM_CODE_MUST_NOT_BE_BLANK = "itemCode must not be blank";
  private static final String ITEM_SKU_MUST_NOT_BE_BLANK = "itemSku must not be blank";

  @Autowired
  private ItemService itemService;

  @Autowired
  private ProductService productService;

  @Autowired
  private PristineService pristineService;

  @Autowired
  private MasterDataService masterDataService;

  @Autowired
  private ItemPickupPointService itemPickupPointService;

  @Autowired
  private ObjectConverterService objectConverterService;

  @Override
  public OfferedSummaryVo getOfferedSummaryByItemCode(String storeId, String username,
      String requestId, String itemCode, String defaultSku) {
    checkArgument(StringUtils.isNotBlank(storeId), OfferedServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemCode),
        OfferedServiceImpl.ITEM_CODE_MUST_NOT_BE_BLANK);

    try {
      Item item = itemService.getItemByItemCode(storeId, itemCode);
      if (Objects.isNull(item)) {
        return new OfferedSummaryVo();
      }
      String productSku = item.getProductSku();
      String itemSku = item.getItemSku();
      String productCode = productService.getProduct(storeId, productSku).getProductCode();

      if (StringUtils.isNotBlank(defaultSku)) {
        Item defaultSkuItem =
            itemService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, defaultSku);
        itemSku = denullify(getIfNotNull(defaultSkuItem, Item::getItemSku), itemSku);

        if (defaultSkuItem == null) {
          ItemPickupPoint defaultSkuOfflineItem =
              itemPickupPointService.findByStoreIdAndOfflineItemIdAndMarkForDeleteFalse(storeId, defaultSku);
          itemSku = denullify(getIfNotNull(defaultSkuOfflineItem, ItemPickupPoint::getItemSku), itemSku);
        }
      }

      Set<String> productCodes = new HashSet<>();
      productCodes.add(productCode);
      Map<String, MasterDataProductAndItemsVO> masterDataProducts = masterDataService
          .getMasterDataProductDetailResponse(storeId, username, requestId, productCodes, false);

      return objectConverterService.convertMasterDataProductAndItemsVoToOfferPageHeaderVo(
          masterDataProducts.get(productCode), itemCode, itemSku);

    } catch (Exception e) {
      LOG.error("failed to getOfferedSummaryByItemCode : ", ErrorCategory.DATA_NOT_FOUND, e);
      return new OfferedSummaryVo();
    }
  }

  @Override
  public OfferedSummaryVo getOfferedComboSummary(MandatoryRequestParam param, String pristineId,
      String itemCode, String itemSku, String defaultSku) throws Exception {
    if (StringUtils.isNotBlank(pristineId)) {
      return objectConverterService.convertPristineItemDetailAndMappingVoToOfferedSummaryVo(
          pristineService.getPristineItemsMappingByPristineId(param, pristineId, itemSku));
    } else if (StringUtils.isNotBlank(itemCode)) {
      return this.getOfferedSummaryByItemCode(param.getStoreId(), param.getUsername(),
          param.getRequestId(), itemCode, defaultSku);
    }

    return getOfferedSummaryVo(param, itemSku, defaultSku);
  }

  @Override
  public OfferedSummaryVo getOfferedSummary(MandatoryRequestParam param, String pristineId,
      String itemCode, String itemSku, String defaultSku) throws Exception {
    if (StringUtils.isNotBlank(pristineId)) {
      return objectConverterService.convertPristineItemDetailAndMappingVoToOfferedSummaryVo(
          pristineService.getPristineItemsMappingByPristineId(param, pristineId, itemSku));
    } else if (StringUtils.isNotBlank(itemCode)) {
      return this.getOfferedSummaryByItemCode(param.getStoreId(), param.getUsername(),
          param.getRequestId(), itemCode, defaultSku);
    } else{
      throw new ApplicationRuntimeException();
    }
  }

  private OfferedSummaryVo getOfferedSummaryVo(MandatoryRequestParam param, String itemSku,
      String defaultSku) throws Exception {
    checkArgument(StringUtils.isNotBlank(itemSku), OfferedServiceImpl.ITEM_SKU_MUST_NOT_BE_BLANK);
    Item item = this.itemService
        .getItem(param.getStoreId(), param.getRequestId(), param.getUsername(), itemSku, true,
            false, false, false, null, false, false);

    if (StringUtils.isNotBlank(item.getItemCode()) || item.isSynchronized()) {
      String itemCode = item.getItemCode();
      return this.getOfferedSummaryByItemCode(param.getStoreId(), param.getUsername(),
          param.getRequestId(), itemCode, defaultSku);
    }

    String productSku = item.getProductSku();
    Product product = this.productService.getProduct(param.getStoreId(), productSku);

    return objectConverterService.convertProductToOfferedSummaryVo(param, product, item);
  }
}
