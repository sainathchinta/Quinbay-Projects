package com.gdn.x.product.service.util;

import static com.gdn.common.base.GdnPreconditions.checkArgument;
import static java.util.stream.Collectors.toList;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import com.gdn.x.product.model.vo.AiGeneratedFieldsResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;

import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.businesspartner.constant.ProfileFlagNames;
import com.gdn.x.businesspartner.domain.event.model.BusinessPartnerChange;
import com.gdn.x.product.domain.event.model.DiscountPriceModel;
import com.gdn.x.product.domain.event.model.SalesCatalogModel;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.model.entity.Category;
import com.gdn.x.product.model.entity.DiscountPrice;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductAttribute;
import com.gdn.x.product.model.entity.ProductAttributeDetail;
import com.gdn.x.product.model.entity.SalesCatalog;
import com.gdn.x.product.model.entity.BusinessPartner;
import com.gdn.x.product.model.vo.ItemCatalogVO;
import com.gdn.x.product.model.vo.ItemCatalogVOV2;
import com.gdn.x.product.model.vo.ItemCategoryVO;
import com.gdn.x.product.model.vo.ItemCategoryVOV2;
import com.gdn.x.product.model.vo.PreOrderVO;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;

public class ObjectConverterUtil {

  private static final Logger LOGGER = LoggerFactory.getLogger(ObjectConverterUtil.class);
  private static final String CATEGORY_LIST_MUST_NOT_BE_NULL = "categoryCodeList must not be null";

  private ObjectConverterUtil() {
  }

  public static ProductAttribute convertPristineAttributeToProductAttribute(
      PristineDataItem pristineDataItem) {
    checkArgument(pristineDataItem != null, "pristineDataItem must not be null");
    List<ProductAttributeDetail> pristineAttributeDetails =
        pristineDataItem.getPristineListingAttributes().entrySet().stream()
            .map(e -> new ProductAttributeDetail(e.getKey(), e.getKey(), e.getValue()))
            .collect(toList());
    return new ProductAttribute(pristineDataItem.getPristineId(), pristineAttributeDetails);
  }

  public static String convertBytesToString(byte[] bytes) {
    if (bytes != null) {
      return new String(bytes);
    } else {
      return null;
    }
  }

  public static List<ItemCatalogVO> convertToListOfItemCatalog(
      List<List<CategoryResponse>> listOfCategoriesList) {
    checkArgument(listOfCategoriesList != null, CATEGORY_LIST_MUST_NOT_BE_NULL);
    List<ItemCatalogVO> itemCatalogs = new ArrayList<>();
    for (List<CategoryResponse> categoriesList : listOfCategoriesList) {
      List<ItemCategoryVO> itemCategoryList = new ArrayList<>();
      for (CategoryResponse category : categoriesList) {
        ItemCategoryVO itemCategory = new ItemCategoryVO();
        itemCategory.setCategory(category.getName());
        itemCategory.setCategoryId(category.getCategoryCode());
        itemCategory.setProductCategoryCode(category.getCategoryCode());
        itemCategory.setLevel(categoriesList.size() - categoriesList.indexOf(category));
        itemCategory.setDocumentType(category.getDocumentType());
        itemCategory.setActivated(category.isActivated());
        itemCategory.setCategoryActive(category.isActivated());
        itemCategory.setDisplay(category.isDisplay());
        itemCategoryList.add(itemCategory);
      }
      if (!categoriesList.isEmpty()) {
        String catalogCode = categoriesList.get(0).getCatalog().getCatalogCode();
        itemCatalogs.add(new ItemCatalogVO(catalogCode, itemCategoryList));
      }
    }
    return itemCatalogs;
  }

  public static List<ItemCatalogVOV2> convertToListOfItemCatalogV2(
      List<List<CategoryResponse>> listOfCategoriesList) {
    checkArgument(listOfCategoriesList != null, CATEGORY_LIST_MUST_NOT_BE_NULL);
    listOfCategoriesList =
        listOfCategoriesList.stream().filter(Objects::nonNull).collect(toList());
    List<ItemCatalogVOV2> itemCatalogs = new ArrayList<>();
    for (List<CategoryResponse> categoriesList : listOfCategoriesList) {
      List<ItemCategoryVOV2> itemCategoryList = new ArrayList<>();
      for (CategoryResponse category : categoriesList) {
        ItemCategoryVOV2 itemCategory = new ItemCategoryVOV2();
        itemCategory.setCategory(category.getName());
        itemCategory.setCategoryId(category.getCategoryCode());
        itemCategory.setProductCategoryCode(category.getCategoryCode());
        itemCategory.setLevel(categoriesList.size() - categoriesList.indexOf(category));
        itemCategory.setDocumentType(category.getDocumentType());
        itemCategory.setActivated(category.isActivated());
        itemCategory.setCategoryActive(category.isActivated());
        itemCategory.setDisplay(category.isDisplay());
        itemCategoryList.add(itemCategory);
      }
      if (!categoriesList.isEmpty()) {
        String catalogCode = categoriesList.get(0).getCatalog().getCatalogCode();
        itemCatalogs.add(new ItemCatalogVOV2(catalogCode, itemCategoryList));
      }
    }
    return itemCatalogs;
  }

  public static MandatoryRequestParam generateMandatoryRequestParam(
      BusinessPartnerChange businessPartnerChange) throws Exception {
    checkArgument(businessPartnerChange != null, "businessPartnerChange must not be null");
    return MandatoryRequestParam.generateMandatoryRequestParam(businessPartnerChange.getStoreId(),
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
        businessPartnerChange.getUpdatedBy(), businessPartnerChange.getUpdatedBy(),
        Constants.DEFAULT_CHANNEL_ID);
  }

  public static Category toCategoryList(String categoryCode) {
    Category category = new Category();
    category.setCategoryCode(categoryCode);
    return category;
  }

  public static DiscountPriceModel convertToDiscountPriceModel(DiscountPrice discountPrice) {
    DiscountPriceModel discountPriceModel = new DiscountPriceModel();
    discountPriceModel.setDiscountPrice(discountPrice.getDiscountPrice());
    discountPriceModel.setStartDateTime(discountPrice.getStartDateTime());
    discountPriceModel.setEndDateTime(discountPrice.getEndDateTime());
    discountPriceModel.setAdjustmentName(discountPrice.getAdjustmentName());
    discountPriceModel.setAdjustmentType(discountPrice.getAdjustmentType());
    discountPriceModel.setCampaignCode(discountPrice.getCampaignCode());
    return discountPriceModel;
  }

  public static DiscountPrice toMerchantPromoDiscountPrice(
      DiscountPriceModel merchantPromoDiscountPrice) {
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setDiscountPrice(merchantPromoDiscountPrice.getDiscountPrice());
    discountPrice.setStartDateTime(merchantPromoDiscountPrice.getStartDateTime());
    discountPrice.setEndDateTime(merchantPromoDiscountPrice.getEndDateTime());
    discountPrice.setAdjustmentName(merchantPromoDiscountPrice.getAdjustmentName());
    discountPrice.setCampaignCode(merchantPromoDiscountPrice.getCampaignCode());
    return discountPrice;
  }

  public static boolean checkPreOrderDateStatus(Date preOrderDate) {
    Date currentDate = new Date();
    try {
      SimpleDateFormat simpleDateFormat = new SimpleDateFormat(Constants.DATE_FORMAT);
      currentDate = simpleDateFormat.parse(simpleDateFormat.format(currentDate));
    } catch (Exception e) {
      LOGGER.error("Error while parsing PreOrderDate : {} with error : ", currentDate, e);
      return true;
    }
    if (currentDate.equals(preOrderDate) || currentDate.after(preOrderDate)) {
      return true;
    }
    return false;
  }

  public static PreOrderVO checkPreOrder(Product product) {
    PreOrderVO preOrderVO = new PreOrderVO();
    if (Objects.nonNull(product.getPreOrder()) && Boolean.TRUE.equals(product.getPreOrder().getIsPreOrder())) {
      BeanUtils.copyProperties(product.getPreOrder(), preOrderVO);
      if (Constants.WEEK.equals(product.getPreOrder().getPreOrderType())) {
        preOrderVO.setPreOrderType(Constants.DAYS);
        preOrderVO.setPreOrderValue(product.getPreOrder().getPreOrderValue() * Constants.NUM_OF_DAYS_IN_WEEK);
      } else if (Constants.DAYS.equals(product.getPreOrder().getPreOrderType())) {
        preOrderVO.setPreOrderType(Constants.DAYS);
        preOrderVO.setPreOrderValue(product.getPreOrder().getPreOrderValue());
      } else if (checkPreOrderDateStatus(product.getPreOrder().getPreOrderDate())) {
        preOrderVO.setIsPreOrder(false);
      }
    } else {
      preOrderVO.setIsPreOrder(false);
    }
    return preOrderVO;
  }

  public static List<SalesCatalog> convertToSalesCatalogsFromSalesCatalogsModel(
      List<SalesCatalogModel> salesCatalogModels) {
    List<SalesCatalog> salesCatalogList = new ArrayList<>();
    for (SalesCatalogModel salesCatalogModel : salesCatalogModels) {
      SalesCatalog salesCatalogEntity = new SalesCatalog();
      salesCatalogEntity.setCatalogCode(salesCatalogModel.getCatalogCode());
      salesCatalogEntity.setListOfCategories(salesCatalogModel.getCategoryCodes().stream()
          .map(ObjectConverterUtil::toCategoryList).collect(toList()));
      salesCatalogList.add(salesCatalogEntity);
    }
    return salesCatalogList;
  }

  /**
   * Converts BusinessPartnerChange to BusinessPartner.
   * 
   * @param businessPartnerChange the business partner change object
   * @param businessPartner the existing business partner (can be null)
   * @return the converted business partner
   */
  public static BusinessPartner convertBusinessPartnerChangeToBusinessPartner(
          BusinessPartnerChange businessPartnerChange, BusinessPartner businessPartner) {
    if (Objects.nonNull(businessPartnerChange.getCompany())) {
      businessPartner = Optional.ofNullable(businessPartner).orElse(new BusinessPartner());
      businessPartner.setBusinessPartnerCode(businessPartnerChange.getBusinessPartnerCode());
      businessPartner.setBusinessPartnerType(businessPartnerChange.getBusinessPartnerType());
      businessPartner.setMerchantStatus(businessPartnerChange.getMerchantStatus());
      businessPartner.setAllCategory(businessPartnerChange.isAllCategory());
      businessPartner.setBusinessPartnerName(businessPartnerChange.getCompany().getBusinessPartnerName());
      businessPartner.setName(businessPartnerChange.getCompany().getName());
      businessPartner.setMerchantType(businessPartnerChange.getCompany().getMerchantType());
      businessPartner.setInternationalFlag(businessPartnerChange.getCompany().isInternationalFlag());
      businessPartner.setLinkedPartnerStore(businessPartnerChange.getCompany().getLinkedPartnerStore());
      businessPartner.setUmkmFlag(businessPartnerChange.getCompany().isUmkmFlag());
      businessPartner.setOfflineToOnlineFlag(businessPartnerChange.getCompany().isOfflineToOnlineFlag());
      businessPartner.setInventoryFulfillment(businessPartnerChange.getCompany().getInventoryFulfillment());
      businessPartner.setCncActivated(businessPartnerChange.getCompany().isCncActivated());
      businessPartner.setBusinessPartnerAlias(businessPartnerChange.getCompany().getBusinessPartnerAlias());
      businessPartner.setSupplierFlag(businessPartnerChange.getCompany().isSupplierFlag());
      businessPartner.setMerchantFlag(businessPartnerChange.getCompany().isMerchantFlag());
      businessPartner.setCustomerFlag(businessPartnerChange.getCompany().isCustomerFlag());
      businessPartner.setMerchantDeliveryType(businessPartnerChange.getCompany().getMerchantDeliveryType());
      businessPartner.setStoreId(businessPartnerChange.getStoreId());
      businessPartner.setSalesChannel(
          Optional.ofNullable(businessPartnerChange.getCompany().getSalesChannel()).orElseGet(ArrayList::new));
      businessPartner.setSellerOmg(
          (Boolean) Optional.ofNullable(businessPartnerChange.getFlags()).orElse(new HashMap<>())
              .get(ProfileFlagNames.BLIBLI_OMG));
    }
    return businessPartner;
  }


  public static AiGeneratedFieldsResponse getAiGeneratedFields(
      ProductDetailResponse productDetailResponse) {
    return Optional.ofNullable(productDetailResponse.getAiGeneratedFieldsResponse()).map(source -> {
      AiGeneratedFieldsResponse aiGeneratedFieldsResponse = new AiGeneratedFieldsResponse();
      aiGeneratedFieldsResponse.setAiGeneratedBrand(source.isAiGeneratedBrand());
      aiGeneratedFieldsResponse.setAiGeneratedCategory(source.isAiGeneratedCategory());
      return aiGeneratedFieldsResponse;
    }).orElseGet(AiGeneratedFieldsResponse::new);
  }
}

