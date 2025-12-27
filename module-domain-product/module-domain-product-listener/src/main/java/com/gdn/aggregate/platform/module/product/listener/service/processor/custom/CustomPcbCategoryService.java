package com.gdn.aggregate.platform.module.product.listener.service.processor.custom;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.platform.module.product.listener.model.custom.CustomPcbCategory;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Category;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Product;
import com.gdn.aggregate.platform.module.product.listener.repository.custom.CustomPcbCategoryRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component("ProductCustomPcbCategoryService")
public class CustomPcbCategoryService {

  @Autowired
  private CustomPcbCategoryRepository pcbCategoryRepository;

  public List<Category> getBreadCrumbMasterCategoryCodes(Product product) {
    return Optional.ofNullable(product)
        .map(this::toMasterCategoryCode)
        .map(this::getBreadCrumbCategories)
        .orElse(null);
  }

  public List<Category> getBreadCrumbSalesCategoryCodes(Product product) {
    return Optional.ofNullable(product)
        .map(this::toListSalesCategoryCode)
        .orElseGet(ArrayList::new)
        .stream()
        .map(this::getBreadCrumbCategories)
        .filter(Objects::nonNull)
        .flatMap(List::stream)
        .filter(Objects::nonNull)
        .collect(Collectors.toList());
  }

  public List<Category> getBreadCrumbCategories(String categoryCode) {
    return Optional.ofNullable(categoryCode)
        .map(this::getCurrentLevelPcbCategory)
        .map(CustomPcbCategory::getBreadCrumbCategories)
        .map(this::toSivaCategories)
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

  public String getBreadCrumbSales(Product product) {
    return Optional.ofNullable(toSalesCategoryCode(product))
        .map(this::getBreadCrumb)
        .orElse(null);
  }

  public String getBreadCrumb(String categoryCode) {
    return Optional.ofNullable(categoryCode)
        .map(this::getCurrentLevelPcbCategory)
        .map(CustomPcbCategory::getBreadcrumb)
        .orElse(null);
  }

  private CustomPcbCategory getTopLevelPcbCategory(String categoryCode) {
    return Optional.ofNullable(categoryCode)
        .map(this::getCurrentLevelPcbCategory)
        .map(CustomPcbCategory::getTopCategoryId)
        .flatMap(pcbCategoryRepository::findById)
        .orElse(null);
  }

  private CustomPcbCategory getCurrentLevelPcbCategory(String categoryCode) {
    return Optional.ofNullable(categoryCode)
        .map(pcbCategoryRepository::findByCategoryCode)
        .orElse(null);
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

  private String toMasterCategoryCode(Product product) {
    return Optional.ofNullable(product)
        .map(Product::getMasterCatalog)
        .map(Product.MasterCatalog::getCategory)
        .map(Product.Category::getCategoryCode)
        .orElse(null);
  }

  public Category getTopLevelSalesCategory(Product product) {
    return Optional.ofNullable(product)
        .map(this::toSalesCategoryCode)
        .map(this::getTopLevelPcbCategory)
        .map(this::toSivaCategory)
        .orElse(null);
  }

  public Category getCurrentLevelSalesCategory(Product product) {
    return Optional.ofNullable(product)
        .map(this::toSalesCategoryCode)
        .map(this::getCurrentLevelPcbCategory)
        .map(this::toSivaCategory)
        .orElse(null);
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

}
