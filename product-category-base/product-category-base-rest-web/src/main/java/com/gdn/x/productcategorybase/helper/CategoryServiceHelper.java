package com.gdn.x.productcategorybase.helper;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.gdn.common.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Component;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.web.param.PageableHelper;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.productcategorybase.dto.response.CatalogResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.service.CategoryService;

/**
 * Created by Kesha on 12/07/16.
 */
@Component
public class CategoryServiceHelper {

  @Autowired
  private CategoryService categoryService;

  private static final Logger LOGGER = LoggerFactory.getLogger(CategoryServiceHelper.class);

  ObjectMapper mapper = new ObjectMapper();

  public String getCategoryList(String storeId, Integer page, Integer size, AtomicLong totalCount) throws Exception {
    LOGGER.info("Load Categories from DB for page {} and size {}",page,size);
    Pageable pageable = PageRequest.of(page, size);
    Page<Category> categoryPage = this.categoryService.findByStoreId(storeId, pageable);
    List<CategoryResponse> categoryResponses = new ArrayList<>();
    for (Category category : categoryPage.getContent()) {
      categoryResponses.add(parseCategoryResponse(category));
    }
    totalCount.set(categoryPage.getTotalElements());
    return this.mapper.writeValueAsString(categoryResponses);
  }

  public static CategoryResponse parseCategoryResponse(Category category) {
    CategoryResponse response = new CategoryResponse();
    BeanUtils.copyProperties(category, response);
    CatalogResponse catalog = new CatalogResponse();
    BeanUtils.copyProperties(category.getCatalog(), catalog);
    catalog.setCatalogType(category.getCatalog().getCatalogType().toString());
    response.setCatalog(catalog);
    if (category.getParentCategory() != null) {
      response.setParentCategoryId(category.getParentCategory().getId());
    }
    return response;
  }

  public GdnRestListResponse<CategoryResponse> findByNameWithChildCount(String requestId, String storeId, String name,
      Pageable pageable, String state, String documentFilterType) {
    Page<Category> categoryPage = categoryService.findByName(storeId, name, pageable, state, documentFilterType);
    List<CategoryResponse> categoryResponses = new ArrayList<>();
    for (Category category : categoryPage.getContent()) {
      CategoryResponse response = parseCategoryResponse(category);
      response.setChildCount(categoryService.findActiveChildCountForParent(storeId, category));
      categoryResponses.add(response);
    }
    return new GdnRestListResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, true, categoryResponses,
        new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(), categoryPage.getTotalElements()), requestId);
  }
}
