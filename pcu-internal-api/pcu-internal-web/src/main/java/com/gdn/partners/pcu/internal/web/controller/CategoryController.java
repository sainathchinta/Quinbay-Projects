package com.gdn.partners.pcu.internal.web.controller;

import java.io.IOException;
import java.util.List;

import jakarta.validation.Valid;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.partners.pcu.internal.service.CategoryHistoryService;
import com.gdn.partners.pcu.internal.web.model.response.CategoryHistoryWebResponse;
import org.hibernate.validator.constraints.NotBlank;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.Metadata;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.CategoryControllerPath;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.service.CategoryService;
import com.gdn.x.productcategorybase.dto.response.CategoryTreeNodeResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Tag(name = "Category API")
@RestController
@RequestMapping(value = CategoryControllerPath.BASE_PATH)
@Validated
public class CategoryController {

  private static final String RESTRICTED_KEYWORD_UPSERT = "RESTRICTED_KEYWORD_UPSERT";
  private static final String RESTRICTED_KEYWORD_DELETE = "RESTRICTED_KEYWORD_DELETE";
  @Autowired
  private ClientParameterHelper clientParameterHelper;

  @Autowired
  private CategoryService categoryService;

  @Autowired
  private CategoryHistoryService categoryHistoryService;

  @Operation(summary = "Returns complete category tree with each review config of each")
  @GetMapping(value = CategoryControllerPath.FETCH_CATEGORY_TREE, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<CategoryTreeNodeResponse> fetchCategoryTreeWithReviewConfig() {
    log.info("Retrieving category tree with review configs");
    List<CategoryTreeNodeResponse> categoryTreeNodeList = this.categoryService.fetchCategoryTreeWithReviewConfig();
    return new ListBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        categoryTreeNodeList, new Metadata(0, categoryTreeNodeList.size(), Long.valueOf(categoryTreeNodeList.size())));
  }

  @Operation(summary = "Bulk restricted keyword upload")
  @PostMapping(value = CategoryControllerPath.RESTRICTED_KEYWORD_BULK_UPLOAD, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse bulkRestrictedKeywordUpload(@PathVariable("processType")
  @Valid @NotBlank(message = ErrorMessages.RESTRICTED_KEYWORD_BULK_UPLOAD_EMPTY) String processType,
      @RequestParam MultipartFile request) throws Exception {
    String requestId = clientParameterHelper.getRequestId();
    GdnPreconditions.checkArgument(
        RESTRICTED_KEYWORD_UPSERT.equals(processType) || RESTRICTED_KEYWORD_DELETE.equals(processType),
        ErrorMessages.INVALID_RESTRICTED_KEYWORD_TYPE);
    log.info("Bulk restricted keyword upload for type {} and requestId {}", processType, requestId);
    try {
      categoryService.saveRestrictedKeywordFile(request, processType, requestId, clientParameterHelper.getStoreId(),
          clientParameterHelper.getUsername());
    } catch (IOException e) {
      return new BaseResponse("Error when transferring file " + request.getOriginalFilename() + e.getMessage(), null,
          false, requestId);
    }
    return new BaseResponse(null, null, true, requestId);
  }

  @Operation(summary = "Get category history by category code")
  @GetMapping(value = CategoryControllerPath.HISTORY, produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnRestListResponse<CategoryHistoryWebResponse> getCategoryHistory(
      @RequestParam("categoryCode") String categoryCode,
      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "10") int size) {
    log.info("Fetching category history for categoryCode: {}", categoryCode);
    Page<CategoryHistoryWebResponse> response =
        categoryHistoryService.categoryHistory(clientParameterHelper.getStoreId(), categoryCode,
            page, size);
    return new GdnRestListResponse<>(response.getContent(),
        new PageMetaData(size, page, response.getTotalElements()),
        clientParameterHelper.getRequestId());
  }
}
