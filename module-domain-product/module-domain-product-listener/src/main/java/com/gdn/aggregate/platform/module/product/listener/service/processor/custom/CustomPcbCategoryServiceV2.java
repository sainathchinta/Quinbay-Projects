package com.gdn.aggregate.platform.module.product.listener.service.processor.custom;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomPcbCategory;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Product;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Category;
import com.gdn.aggregate.platform.module.product.listener.repository.custom.CustomPcbCategoryRepository;

@Component("ProductCustomPcbCategoryServiceV2")
public class CustomPcbCategoryServiceV2 {

  @Autowired
  private CustomPcbCategoryRepository customPcbCategoryRepository;

  public List<CustomPcbCategory> findAllByCategoryCode(Set<String> categoryCodes) {
    return customPcbCategoryRepository.findByCategoryCodeIn(categoryCodes);
  }

  public Category getTopLevelSalesCategory(Product product, List<CustomPcbCategory> allCustomPcbCategory) {
    return Optional.ofNullable(product)
        .map(this::toSalesCategoryCode)
        .map(categoryCode -> getTopLevelPcbCategory(categoryCode, allCustomPcbCategory))
        .map(this::toSivaCategory)
        .orElse(null);
  }

  public Category getCurrentLevelSalesCategory(Product product, List<CustomPcbCategory> allCustomPcbCategory) {
    return Optional.ofNullable(product)
        .map(this::toSalesCategoryCode)
        .map(categoryCode -> getCurrentLevelPcbCategory(categoryCode, allCustomPcbCategory))
        .map(this::toSivaCategory)
        .orElse(null);
  }

  public String getBreadCrumbSales(Product product, List<CustomPcbCategory> allCustomPcbCategory) {
    return Optional.ofNullable(toSalesCategoryCode(product))
        .map(catCode -> getBreadCrumb(catCode, allCustomPcbCategory))
        .orElse(null);
  }
  public String getBreadCrumb(String categoryCode, List<CustomPcbCategory> allCustomPcbCategory) {
    return Optional.ofNullable(categoryCode)
        .map(catCode -> getCurrentLevelPcbCategory(categoryCode, allCustomPcbCategory))
        .map(CustomPcbCategory::getBreadcrumb)
        .orElse(null);
  }

  public List<Category> getBreadCrumbMasterCategoryCodes(Product product, List<CustomPcbCategory> allCustomPcbCategory) {
    return Optional.ofNullable(product)
        .map(this::toMasterCategoryCode)
        .map(catCode -> getBreadCrumbCategories(catCode, allCustomPcbCategory))
        .orElse(null);
  }

  public List<Category> getBreadCrumbSalesCategoryCodes(Product product, List<CustomPcbCategory> allCustomPcbCategory) {
    return Optional.ofNullable(product)
        .map(this::toListSalesCategoryCode)
        .orElseGet(ArrayList::new)
        .stream()
        .map(catCode -> getBreadCrumbCategories(catCode, allCustomPcbCategory))
        .filter(Objects::nonNull)
        .flatMap(List::stream)
        .filter(Objects::nonNull)
        .collect(Collectors.toList());
  }

  public List<Category> getBreadCrumbCategories(String categoryCode, List<CustomPcbCategory> allCustomPcbCategory) {
    return Optional.ofNullable(categoryCode)
        .map(catCode -> getCurrentLevelPcbCategory(categoryCode,  allCustomPcbCategory))
        .map(CustomPcbCategory::getBreadCrumbCategories)
        .map(this::toSivaCategories)
        .orElse(null);
  }

  private String toMasterCategoryCode(Product product) {
    return Optional.ofNullable(product)
        .map(Product::getMasterCatalog)
        .map(Product.MasterCatalog::getCategory)
        .map(Product.Category::getCategoryCode)
        .orElse(null);
  }

  private List<String> toListSalesCategoryCode(Product product) {
    return Optional.ofNullable(product)
        .map(Product::getSalesCatalogs)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .map(Product.SalesCatalog::getListOfCategories)
        .filter(Objects::nonNull)
        .flatMap(List::stream)
        .filter(Objects::nonNull)
        .map(Product.Category::getCategoryCode)
        .filter(Objects::nonNull)
        .collect(Collectors.toList());
  }

  private String toSalesCategoryCode(Product product) {
    return Optional.ofNullable(product)
        .map(Product::getSalesCatalogs)
        .map(MainUtil::toListFirstData)
        .map(Product.SalesCatalog::getListOfCategories)
        .map(MainUtil::toListFirstData)
        .map(Product.Category::getCategoryCode)
        .orElse(null);
  }


  private CustomPcbCategory getTopLevelPcbCategory(String categoryCode, List<CustomPcbCategory> allCustomPcbCategory) {
    return Optional.ofNullable(categoryCode)
        .map(catCode -> getCurrentLevelPcbCategory(catCode, allCustomPcbCategory))
        .map(CustomPcbCategory::getTopCategoryId)
        .map(id -> findById(id, allCustomPcbCategory))
        .orElse(null);
  }

  private CustomPcbCategory findById(String id, List<CustomPcbCategory> allCustomPcbCategory) {
    return Optional.ofNullable(allCustomPcbCategory).orElseGet(ArrayList::new)
        .stream()
        .filter(customPcbCategory -> Optional.ofNullable(id).orElse(StringUtils.EMPTY).equals(customPcbCategory.getId()))
        .findFirst().orElse(null);
  }

  private CustomPcbCategory getCurrentLevelPcbCategory(String categoryCode, List<CustomPcbCategory> allCustomPcbCategory) {
    return Optional.ofNullable(allCustomPcbCategory).orElseGet(ArrayList::new)
        .stream()
        .filter(customPcbCategory -> Optional.ofNullable(categoryCode).orElse(StringUtils.EMPTY).equals(customPcbCategory.getCategoryCode()))
        .findFirst().orElse(null);
  }

  private Category toSivaCategory(CustomPcbCategory pcbCategory) {
    return Optional.ofNullable(pcbCategory)
        .map(val -> Category.builder()
            .id(val.getId())
            .code(val.getCategoryCode())
            .name(val.getName())
            .level(val.getLevel())
            .build())
        .orElse(null);
  }

  private List<Category> toSivaCategories(List<CustomPcbCategory.MiniPcbCategory> breadCrumbCategories) {
    return Optional.ofNullable(breadCrumbCategories)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .map(this::toSivaCategory)
        .filter(Objects::nonNull)
        .collect(Collectors.toList());
  }

  private Category toSivaCategory(CustomPcbCategory.MiniPcbCategory pcbCategory) {
    return Optional.ofNullable(pcbCategory)
        .map(val -> Category.builder()
            .id(val.getId())
            .code(val.getCategoryCode())
            .name(val.getName())
            .level(val.getLevel())
            .build())
        .orElse(null);
  }
}
