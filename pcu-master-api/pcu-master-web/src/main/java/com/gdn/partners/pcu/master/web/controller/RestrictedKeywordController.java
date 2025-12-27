package com.gdn.partners.pcu.master.web.controller;


import java.util.List;

import com.gdn.partners.pcu.master.web.model.request.RestrictedKeywordsSearchWebRequest;
import com.gdn.partners.pcu.master.web.model.response.RestrictedKeywordsWebResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.Metadata;
import com.gdn.partners.pcu.master.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.master.model.RestrictedKeywordApiPath;
import com.gdn.partners.pcu.master.service.RestrictedKeywordService;
import com.gdn.partners.pcu.master.web.model.response.RestrictedKeywordHistoryWebResponse;
import com.gdn.partners.pcu.master.web.model.response.RestrictedKeywordsListingWebResponse;
import com.gdn.x.productcategorybase.dto.request.RestrictedKeywordsUpdateRequest;
import com.gdn.partners.pcu.master.web.model.response.UiValidationRestrictedKeywordsWebResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Tag(name = "RestrictedKeyword API")
@RestController
@RequestMapping(value = RestrictedKeywordApiPath.BASE_PATH)
public class RestrictedKeywordController {

  @Autowired
  private RestrictedKeywordService restrictedKeywordService;

  @Autowired
  private ClientParameterHelper clientParameterHelper;

  @Operation(summary = "Getting restricted keyword history")
  @GetMapping(value = RestrictedKeywordApiPath.HISTORY, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<RestrictedKeywordHistoryWebResponse> getRestrictedKeywordHistory(
      @PathVariable String keywordId, @RequestParam(defaultValue = "0") int page,
      @RequestParam(defaultValue = "10") int size) {
    String requestId = this.clientParameterHelper.getRequestId();
    log.info("Getting restricted keyword history. requestId : {} , keywordId : {} ", requestId, keywordId);
    Page<RestrictedKeywordHistoryWebResponse> responses =
        restrictedKeywordService.getRestrictedKeywordHistory(keywordId, page, size);
    return new ListBaseResponse<>(null, null, true, requestId, responses.getContent(),
        new Metadata(page, size, responses.getTotalElements()));
  }

  @Operation(summary = "Add/Delete/Update Keywords")
  @PostMapping(value = RestrictedKeywordApiPath.UPSERT, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse upsertRestrictedKeyword(
      @RequestBody RestrictedKeywordsUpdateRequest restrictedKeywordsUpdateRequest) {
      log.info("Restricted Keyword upsert with requestId {} and request {} ", clientParameterHelper.getRequestId(), restrictedKeywordsUpdateRequest);
      return restrictedKeywordService.upsertRestrictedKeyword(restrictedKeywordsUpdateRequest);
  }

  @Operation(summary = "Getting ui validation restricted keywords")
  @GetMapping(value = RestrictedKeywordApiPath.UI_VALIDATION, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<UiValidationRestrictedKeywordsWebResponse> getUiValidationRestrictedKeywords() {
    String requestId = this.clientParameterHelper.getRequestId();
    log.info("Getting ui validation restricted keywords. requestId : {} ", requestId);
    List<UiValidationRestrictedKeywordsWebResponse> responseList =
        restrictedKeywordService.getUiValidationRestrictedKeywords().getContent();
    return new ListBaseResponse<>(null, null, true, requestId, responseList,
        new Metadata(0, responseList.size(), (long) responseList.size()));
  }

  @Operation(summary = "Search restricted keywords")
  @PostMapping(value = RestrictedKeywordApiPath.SEARCH, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<RestrictedKeywordsWebResponse> getRestrictedKeywords(
    @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "10") int size,
    @RequestBody RestrictedKeywordsSearchWebRequest request) {
    log.info("Getting restricted keywords matching the keyword {} ", request);
    Page<RestrictedKeywordsWebResponse> responses =
      restrictedKeywordService.getRestrictedKeywords(request, page, size);
    return new ListBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
      responses.getContent(), new Metadata(page, size, responses.getTotalElements()));
  }


  @Operation(summary = "Getting Restricted Keyword For Listing")
  @GetMapping(value = RestrictedKeywordApiPath.LISTING, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<RestrictedKeywordsListingWebResponse> getRestrictedKeywordForListing(
      @RequestParam(required = false) String keyword, @RequestParam(defaultValue = "0") int page,
      @RequestParam(defaultValue = "10") int size) {
    String requestId = this.clientParameterHelper.getRequestId();
    log.info("Getting Restricted Keyword For Listing. requestId : {} , keyword : {} ", requestId, keyword);
    Page<RestrictedKeywordsListingWebResponse> response =
        restrictedKeywordService.getRestrictedKeywordForListing(keyword, page, size);
    return new ListBaseResponse<>(null, null, true, requestId, response.getContent(),
        new Metadata(page, size, response.getTotalElements()));
  }

}
