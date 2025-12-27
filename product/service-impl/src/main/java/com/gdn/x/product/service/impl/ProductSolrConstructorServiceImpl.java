package com.gdn.x.product.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.DistributionStatus;
import com.gdn.x.product.model.entity.Category;
import com.gdn.x.product.model.entity.MasterCatalog;
import com.gdn.x.product.model.entity.MasterDataProductImage;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.SalesCatalog;
import com.gdn.x.product.model.solr.ProductSolr;
import com.gdn.x.product.outbound.api.ProductCategoryBaseOutbound;
import com.gdn.x.product.rest.web.model.FieldValueObject;
import com.gdn.x.product.service.api.BusinessPartnerService;
import com.gdn.x.product.service.api.ProductHelperService;
import com.gdn.x.product.service.api.ProductSolrConstructorService;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.product.service.util.CommonUtil;
import com.gdn.x.productcategorybase.domain.event.model.CategoryDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ImageDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductDomainEventModel;
import com.gdn.x.productcategorybase.dto.response.ImageResponse;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class ProductSolrConstructorServiceImpl implements ProductSolrConstructorService {

  private static final String ITEM_NOT_FOUND_FOR_ITEM_CODE = "#solrFailure item not found for item code : ";

  private static final String XPRODUCT_SOLR_REINDEX = "xproduct-solr-reindex";

  @Value("${solr.string.delimiter}")
  private String solrStringDelimiter;

  @Value("${sales.category.catalog.code}")
  private String salesCategoryCatalogCode;

  @Autowired
  private ProductHelperService productHelperService;

  @Autowired
  private BusinessPartnerService businessPartnerService;

  @Autowired
  private ProductCategoryBaseOutbound productCategoryBaseOutbound;

  @Autowired
  private SystemParameterService systemParameterService;

  @Override
  public void constructProduct(ProductSolr productSolr, Product product, boolean getMasterData) {
    productSolr.setProductSku(product.getProductSku());
    productSolr.setProductCode(product.getProductCode());
    productSolr.setSalesCatalog(this.constructAndSetSalesCatalog(product));
    productSolr.setCreatedDate(product.getCreatedDate());
    productSolr.setUpdatedDate(product.getUpdatedDate());
    productSolr.setStoreId(product.getStoreId());
    productSolr.setSynchronized(product.isSynchronized());
    productSolr.setSuspended(product.isSuspended());
    productSolr.setMerchantCode(product.getMerchantCode());
    productSolr.setMarkForDelete(product.isMarkForDelete());
    productSolr.setIsArchived(product.isArchived());
    productSolr.setFreeSample(product.isFreeSample());
    productSolr.setOff2OnChannelActive(product.isOff2OnChannelActive());
    productSolr.setCurationStatus(CommonUtil.getCurationStatus(product));
    productSolr.setBundleProduct(product.isBundleProduct());
    productSolr.setDistributionStatus(
        Optional.ofNullable(product.getDistributionStatus()).orElse(DistributionStatus.NON_DISTRIBUTION).getCode());
    if (Objects.nonNull(product.getProductScore())) {
      productSolr.setProductScoreTotal(
          Math.round(product.getProductScore().getTotalScore() * Constants.ROUND_OFF_FACTOR)
              / Constants.ROUND_OFF_FACTOR);
    }
    try {
      boolean fetchOnlyProductImageDetails = Boolean.parseBoolean(systemParameterService
          .findValueByStoreIdAndVariable(product.getStoreId(), Constants.FETCH_ONLY_IMAGES_DETAILS).getValue());
      if (fetchOnlyProductImageDetails) {
        if (product.isSynchronized()){
          productSolr.setBrand(product.getBrand());
          productSolr.setProductName(product.getProductName());
          List<ImageResponse> imageResponses = new ArrayList<>();
          if (getMasterData) {
            imageResponses = productCategoryBaseOutbound.getProductImagesByProductCode(product.getProductCode());
          }
          productSolr.setProductMainImage(this.setProductMainImage(imageResponses));
          productSolr.setMasterCatalog(this.constructCatalogString(Constants.MASTER_CATALOG, product.getCategoryCode()));
        } else {
          productSolr.setBrand(product.getMasterDataProduct().getBrand());
          productSolr.setProductName(product.getMasterDataProduct().getProductName());
          productSolr.setProductMainImage(this.setProductMainImage(product));
          MasterCatalog selectedMasterCatalog = product.getMasterDataProduct().getMasterCatalog();
          productSolr.setMasterCatalog(this.constructCatalogString(selectedMasterCatalog.getCatalogCode(),
              selectedMasterCatalog.getCategory().getCategoryCode()));
        }
      } else {
        String requestId = UUID.randomUUID().toString();
        Product pcbProduct = new Product();
        BeanUtils.copyProperties(product, pcbProduct);
        pcbProduct = this.productHelperService
            .setMasterDataProductFromMasterData(product.getStoreId(), requestId,
                XPRODUCT_SOLR_REINDEX, pcbProduct);
        if (product.isSynchronized() && getMasterData) {
          log.info("Fetching master data for reindex of product : {}", product.getProductSku());
          product = pcbProduct;
        }
        productSolr.setBrand(product.getMasterDataProduct().getBrand());
        productSolr.setProductName(product.getMasterDataProduct().getProductName());
        productSolr.setProductMainImage(this.setProductMainImage(pcbProduct));
        MasterCatalog selectedMasterCatalog = Objects.nonNull(product.getMasterCatalog()) ?
            product.getMasterCatalog() :
            product.getMasterDataProduct().getMasterCatalog();
        productSolr.setMasterCatalog(this.constructCatalogString(selectedMasterCatalog.getCatalogCode(),
            selectedMasterCatalog.getCategory().getCategoryCode()));
      }
      productSolr.setProductCenterUpdatedDate(product.getProductCenterUpdatedDate());
      productSolr.setCncActive(product.isCncActivated());
      productSolr.setFbbActivated(product.isFbbActivated());
      productSolr.setTradingProduct(product.isTradingProduct());
      productSolr.setB2bActivated(product.isB2bActivated());
      productSolr.setB2cActivated(product.getB2cActivated());
      productSolr.setSizeChartCode(product.getSizeChartCode());
    } catch (Exception e) {
      log.error("Failed on setting master product data fields for product : {}", product.getProductSku(), e);
    }
  }

  private String constructCatalogString(String catalogCode, String categoryCode) {
    return String.format("%s%s%s", catalogCode, this.solrStringDelimiter, categoryCode);
  }

  private List<String> constructAndSetSalesCatalog(Product product) {
    List<String> salesCatalogs = new ArrayList<>();
    for (SalesCatalog salesCatalog : product.getAllSalesCatalogs()) {
      salesCatalogs.addAll(salesCatalog.getListOfCategories().stream()
          .map(category -> (constructCatalogString(salesCatalog.getCatalogCode(), category.getCategoryCode())))
          .collect(Collectors.toList()));
    }
    return salesCatalogs;
  }

  private String setProductMainImage(Product product) {
    MasterDataProductImage result =
        Optional.ofNullable(product.getMasterDataProduct().getMasterDataProductImages()).orElse(new ArrayList<>())
            .stream().filter(MasterDataProductImage::isMainImage).findFirst().orElse(new MasterDataProductImage());
    if (Objects.nonNull(result)) {
      return result.getLocationPath();
    }
    return StringUtils.EMPTY;
  }

  private String setProductMainImage(List<ImageResponse> imageResponses) {
    ImageResponse productImage = Optional.ofNullable(imageResponses).orElse(new ArrayList<>())
            .stream().filter(ImageResponse::isMainImage).findFirst().orElse(null);
    if (Objects.isNull(productImage)) {
      productImage = Optional.ofNullable(imageResponses).orElse(new ArrayList<>())
          .stream().findFirst().orElse(null);
    }
    if (Objects.nonNull(productImage)) {
      return productImage.getLocationPath();
    }
    return StringUtils.EMPTY;
  }

  @Override
  public Map<String, FieldValueObject> constructProductFromMasterDataChanges(ProductSolr productSolr,
      ProductDomainEventModel productDomainEventModel, Map<String, Double> productAndTotalScoreMap) {
    ProductSolr existingProductSolr = new ProductSolr();
    BeanUtils.copyProperties(productSolr, existingProductSolr);
    CategoryDomainEventModel categoryDomainEventModel =
        productDomainEventModel.getProductCategories().get(0).getCategory();
    String catalogCode = categoryDomainEventModel.getCatalog().getCatalogCode();
    if (StringUtils.isBlank(catalogCode)) {
      MasterCatalog masterCatalog =
          this.constructMasterCatalog(productSolr.getMasterCatalog());
      catalogCode = masterCatalog.getCatalogCode();
    }
    productSolr.setMasterCatalog(constructCatalogString(catalogCode,
        categoryDomainEventModel.getCategoryCode()));
    modifySalesCatalog(productSolr, productDomainEventModel);
    if(productSolr.isSynchronized()) {
      productSolr.setProductName(productDomainEventModel.getName());
      productSolr.setBrand(productDomainEventModel.getBrand());
      productSolr.setProductMainImage(setProductMainImageFromMasterData(productDomainEventModel.getImages()));
    }
    if (productAndTotalScoreMap.containsKey(productSolr.getProductSku())) {
      productSolr.setProductScoreTotal(
          Math.round(productAndTotalScoreMap.get(productSolr.getProductSku()) * Constants.ROUND_OFF_FACTOR)
              / Constants.ROUND_OFF_FACTOR);
    }
    Map<String, FieldValueObject> changedFieldValueObjectMap =
        CommonUtil.checkIfSolrUpdateNeededForProductSolr(existingProductSolr, productSolr);
    return changedFieldValueObjectMap.size() > 0 ? changedFieldValueObjectMap : null;
  }

  private String setProductMainImageFromMasterData(List<ImageDomainEventModel> imageDomainEventModelList) {
    ImageDomainEventModel result = Optional.ofNullable(imageDomainEventModelList).orElse(new ArrayList<>())
        .stream().filter(ImageDomainEventModel::isMainImage).findFirst().orElse(null);
    if(Objects.nonNull(result)) {
      return result.getLocationPath();
    }
    return StringUtils.EMPTY;
  }

  private void modifySalesCatalog(ProductSolr productSolr,
      ProductDomainEventModel productDomainEventModel) {
    if (Objects.nonNull(productDomainEventModel.getProductSalesCategoryMapping())) {
      List<String> oldSalesCatalog =
          Optional.ofNullable(productDomainEventModel.getProductSalesCategoryMapping().getOldSalesCategoryCodes())
              .orElse(new ArrayList<>()).stream()
              .map(categoryCode -> constructCatalogString(salesCategoryCatalogCode, categoryCode))
              .collect((Collectors.toList()));
      List<String> newSalesCatalog =
          Optional.ofNullable(productDomainEventModel.getProductSalesCategoryMapping().getNewSalesCategoryCodes())
              .orElse(new ArrayList<>()).stream()
              .map(categoryCode -> constructCatalogString(salesCategoryCatalogCode, categoryCode))
              .collect((Collectors.toList()));
      checkIfMerchantIsUmkmAndAddUmkmSalesCategory(productSolr, productDomainEventModel, newSalesCatalog);
      if (Objects.nonNull(productSolr.getSalesCatalog())) {
        productSolr.getSalesCatalog().removeAll(oldSalesCatalog);
        productSolr.getSalesCatalog().addAll(newSalesCatalog);
      } else {
        productSolr.setSalesCatalog(newSalesCatalog);
      }
    }
  }

  private void checkIfMerchantIsUmkmAndAddUmkmSalesCategory(ProductSolr productSolr,
      ProductDomainEventModel productDomainEventModel, List<String> newSalesCatalog) {
    if (CollectionUtils
        .isNotEmpty(productDomainEventModel.getProductSalesCategoryMapping().getNewUmkmSalesCategoryCodes())
        && businessPartnerService
        .isBusinessPartnerUmkmMerchant(productDomainEventModel.getStoreId(), productSolr.getMerchantCode())) {
      newSalesCatalog.addAll(
          Optional.ofNullable(productDomainEventModel.getProductSalesCategoryMapping().getNewUmkmSalesCategoryCodes())
              .orElse(new ArrayList<>()).stream().map(
              categoryCode -> constructCatalogString(salesCategoryCatalogCode, categoryCode)).collect((Collectors.toList())));
    }
  }

  @Override
  public MasterCatalog constructMasterCatalog(String catalogString) {
    if (StringUtils.isBlank(catalogString)) {
      return null;
    }
    String masterCatalogAttr[] = catalogString.split(this.solrStringDelimiter);
    return new MasterCatalog(masterCatalogAttr[0],
        new Category(masterCatalogAttr[1], masterCatalogAttr[1]));
  }
}
