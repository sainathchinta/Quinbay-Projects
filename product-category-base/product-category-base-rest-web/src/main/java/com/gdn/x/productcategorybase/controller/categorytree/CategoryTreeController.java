package com.gdn.x.productcategorybase.controller.categorytree;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import com.gdn.common.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.productcategorybase.dto.categorytree.CategoryNodeFilterCategoryCodesRequest;
import com.gdn.x.productcategorybase.dto.categorytree.CategoryNodeFilterParentCategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.categorytree.CategoryNodeResponse;
import com.gdn.x.productcategorybase.dto.categorytree.CategoryTreeFilterCategoryCodesRequest;
import com.gdn.x.productcategorybase.entity.categorytree.CategoryNode;
import com.gdn.x.productcategorybase.service.categorytree.CategoryTreeService;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping(value = CategoryTreeControllerPath.BASE_PATH)
@Tag(name = "Category Tree", description = "Category Tree Service API")
public class CategoryTreeController {

  @Autowired
  private CategoryTreeService categoryTreeService;

  @RequestMapping(value = CategoryTreeControllerPath.FILTER_NODE_CATEGORY_CODES, method = RequestMethod.POST,
      consumes = {MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "filter category node by category codes", description = "filter category node by category codes")
  public GdnRestListResponse<CategoryNodeResponse> filterCategoryNodeByCategoryCodes(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody CategoryNodeFilterCategoryCodesRequest request) throws Exception {
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getCatalogCode()),
        CategoryTreeControllerErrorMessage.CATALOG_CODE_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument((request.getCategoryCodes() != null && !request.getCategoryCodes().isEmpty()),
        CategoryTreeControllerErrorMessage.CATEGORY_CODES_MUST_NOT_BE_BLANK);
    List<CategoryNodeResponse> categoryNodeResponses =
        this.findCategoryNodeByCatalogCodeAndCategoryCodesAndActive(request.getCatalogCode(),
            request.getCategoryCodes(), request.isActive());
    return new GdnRestListResponse<CategoryNodeResponse>(categoryNodeResponses, new PageMetaData(
        categoryNodeResponses.size(), 0, categoryNodeResponses.size()), requestId);
  }

  @RequestMapping(value = CategoryTreeControllerPath.FILTER_NODE_PARENT_CATEGORY_CODE, method = RequestMethod.POST,
      consumes = {MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "filter category node by parent category code",
      description = "filter category node by parent category code")
  
  public GdnRestListResponse<CategoryNodeResponse> filterCategoryNodeByParentCategoryCode(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody CategoryNodeFilterParentCategoryCodeRequest request) throws Exception {
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getCatalogCode()),
        CategoryTreeControllerErrorMessage.CATALOG_CODE_MUST_NOT_BE_BLANK);
    List<CategoryNodeResponse> categoryNodeResponses =
        this.findCategoryNodeByCatalogCodeAndParentCategoryCodeAndActive(request.getCatalogCode(),
            request.getParentCategoryCode(), request.isActive());
    return new GdnRestListResponse<CategoryNodeResponse>(categoryNodeResponses, new PageMetaData(
        categoryNodeResponses.size(), 0, categoryNodeResponses.size()), requestId);
  }

  @RequestMapping(value = CategoryTreeControllerPath.FILTER_CATEGORY_CODES, method = RequestMethod.POST,
      consumes = {MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "filter category tree by category codes", description = "filter category tree by category codes")
  
  public GdnRestListResponse<CategoryNodeResponse> filterByCategoryCodes(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody CategoryTreeFilterCategoryCodesRequest request) throws Exception {
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getCatalogCode()),
        CategoryTreeControllerErrorMessage.CATALOG_CODE_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument((request.getCategoryCodes() != null && !request.getCategoryCodes().isEmpty()),
        CategoryTreeControllerErrorMessage.CATEGORY_CODES_MUST_NOT_BE_BLANK);
    List<CategoryNodeResponse> categoryNodeResponses =
        this.findByCatalogCodeAndCategoryCodesAndActive(request.getCatalogCode(), request.getCategoryCodes(),
            request.isActive(), request.isBuildTree());
    return new GdnRestListResponse<CategoryNodeResponse>(categoryNodeResponses, new PageMetaData(
        categoryNodeResponses.size(), 0, categoryNodeResponses.size()), requestId);
  }

  private CategoryNodeResponse generateCategoryNodeResponse(CategoryNode categoryNode) throws Exception {
    CategoryNodeResponse categoryNodeResponse = null;
    if (categoryNode != null) {
      categoryNodeResponse = new CategoryNodeResponse();
      BeanUtils.copyProperties(categoryNode, categoryNodeResponse);
    }
    return categoryNodeResponse;
  }

  private List<CategoryNodeResponse> generateCategoryNodeResponses(List<CategoryNode> categoryNodes) throws Exception {
    List<CategoryNodeResponse> categoryNodeResponses = new ArrayList<CategoryNodeResponse>();
    for (CategoryNode categoryNode : categoryNodes) {
      categoryNodeResponses.add(this.generateCategoryNodeResponse(categoryNode));
    }
    return categoryNodeResponses;
  }

  private List<CategoryNodeResponse> findCategoryNodeByCatalogCodeAndCategoryCodesAndActive(String catalogCode,
      List<String> categoryCodes, boolean active) throws Exception {
    List<CategoryNode> categoryNodes =
        this.categoryTreeService.findCategoryNodeByCatalogCodeAndCategoryCodesAndActive(catalogCode, categoryCodes,
            active);
    return this.generateCategoryNodeResponses(categoryNodes);
  }

  private List<CategoryNodeResponse> findCategoryNodeByCatalogCodeAndParentCategoryCodeAndActive(String catalogCode,
      String parentCategoryCode, boolean active) throws Exception {
    List<CategoryNode> categoryNodes =
        this.categoryTreeService.findCategoryNodeByCatalogCodeAndParentCategoryCodeAndActive(catalogCode,
            parentCategoryCode, active);
    return this.generateCategoryNodeResponses(categoryNodes);
  }

  private List<CategoryNodeResponse> findByCatalogCodeAndCategoryCodesAndActive(String catalogCode,
      List<String> categoryCodes, boolean active, boolean buildTree) throws Exception {
    List<CategoryNode> categoryNodes =
        this.categoryTreeService.findByCatalogCodeAndCategoryCodesAndActive(catalogCode, categoryCodes, active);
    List<CategoryNodeResponse> categoryNodeResponses = null;
    if (buildTree) {
      categoryNodeResponses = this.buildCategoryTrees(categoryNodes, null, categoryCodes);
    } else {
      categoryNodeResponses = this.generateCategoryNodeResponses(categoryNodes);
    }
    return categoryNodeResponses;
  }

  private List<CategoryNodeResponse> buildCategoryTrees(List<CategoryNode> categoryNodes,
      CategoryNode parentCategoryNode, List<String> categoryCodes) throws Exception {
    List<CategoryNodeResponse> categoryNodeResponses = new ArrayList<CategoryNodeResponse>();
    for (CategoryNode categoryNode : categoryNodes) {
      if ((parentCategoryNode == null && (StringUtils.isEmpty(categoryNode.getParentCategoryCode()) || categoryCodes
          .contains(categoryNode.getCategoryCode())))
          || (parentCategoryNode != null && parentCategoryNode.getCategoryCode().equals(
              categoryNode.getParentCategoryCode()))) {
        CategoryNodeResponse categoryNodeResponse = this.generateCategoryNodeResponse(categoryNode);
        if (categoryNode.getChildCount() > 0) {
          categoryNodeResponse.setChildren(this.buildCategoryTrees(categoryNodes, categoryNode, null));
        }
        categoryNodeResponses.add(categoryNodeResponse);
      }
    }
    return categoryNodeResponses;
  }
  
}
