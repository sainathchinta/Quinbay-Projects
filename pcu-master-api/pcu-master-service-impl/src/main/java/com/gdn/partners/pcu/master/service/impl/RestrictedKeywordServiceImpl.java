package com.gdn.partners.pcu.master.service.impl;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.partners.pcu.master.model.ErrorMessages;
import com.gdn.partners.pcu.master.service.impl.helper.BeanUtils;
import com.gdn.partners.pcu.master.web.model.request.RestrictedKeywordsSearchWebRequest;
import com.gdn.partners.pcu.master.web.model.response.RestrictedKeywordsWebResponse;
import com.gdn.x.productcategorybase.dto.request.RestrictedKeywordsSearchRequest;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.partners.pcu.master.client.feign.PCBFeign;
import com.gdn.partners.pcu.master.service.RestrictedKeywordService;
import com.gdn.partners.pcu.master.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.master.web.model.response.RestrictedKeywordHistoryWebResponse;
import com.gdn.partners.pcu.master.web.model.response.RestrictedKeywordsListingWebResponse;
import com.gdn.x.productcategorybase.dto.request.RestrictedKeywordsUpdateRequest;
import com.gdn.partners.pcu.master.web.model.response.UiValidationRestrictedKeywordsWebResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordHistoryResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsListingResponse;
import com.gdn.x.productcategorybase.dto.response.UiValidationRestrictedKeywordsResponse;

@Service
public class RestrictedKeywordServiceImpl implements RestrictedKeywordService {

  @Autowired
  private PCBFeign pcbFeign;

  @Override
  public Page<RestrictedKeywordHistoryWebResponse> getRestrictedKeywordHistory(String keywordId, int page, int size) {
    GdnRestListResponse<RestrictedKeywordHistoryResponse> responseGdnRestListResponse =
        pcbFeign.getRestrictedKeywordHistory(keywordId, page, size);
    ResponseHelper.validateResponse(responseGdnRestListResponse);
    List<RestrictedKeywordHistoryWebResponse> restrictedKeywordHistoryWebResponses =
        responseGdnRestListResponse.getContent().stream().map(ResponseHelper::getRestrictedKeywordHistoryWebResponse)
            .collect(Collectors.toList());
    return new PageImpl<>(restrictedKeywordHistoryWebResponses, PageRequest.of(page, size),
        responseGdnRestListResponse.getPageMetaData().getTotalRecords());
  }

  @Override
  public Page<RestrictedKeywordsWebResponse> getRestrictedKeywords(
    RestrictedKeywordsSearchWebRequest request, int page, int size) {
    RestrictedKeywordsSearchRequest keywordsSearchRequest = new RestrictedKeywordsSearchRequest();
    BeanUtils.copyProperties(request, keywordsSearchRequest);
    GdnRestListResponse<RestrictedKeywordsResponse> responseGdnRestListResponse =
      pcbFeign.getRestrictedKeywords(keywordsSearchRequest, page, size);
    ResponseHelper.validateResponse(responseGdnRestListResponse);
    List<RestrictedKeywordsWebResponse> restrictedKeywordWebResponses =
      responseGdnRestListResponse.getContent().stream()
        .map(ResponseHelper::getRestrictedKeywordsWebResponse).collect(Collectors.toList());
    return new PageImpl<>(restrictedKeywordWebResponses, PageRequest.of(page, size),
      responseGdnRestListResponse.getPageMetaData().getTotalRecords());
  }

  @Override
  public GdnBaseRestResponse upsertRestrictedKeyword(RestrictedKeywordsUpdateRequest restrictedKeywordsUpdateRequest) {
    GdnBaseRestResponse response = new GdnBaseRestResponse();
    if (Objects.nonNull(restrictedKeywordsUpdateRequest)) {
      response = pcbFeign.updateRestrictedKeyword(restrictedKeywordsUpdateRequest);
      ResponseHelper.validateResponse(response);
    }
    return response;
  }

  @Override
  public Page<UiValidationRestrictedKeywordsWebResponse> getUiValidationRestrictedKeywords() {
    GdnRestListResponse<UiValidationRestrictedKeywordsResponse> response = pcbFeign.getUiValidationRestrictedKeywords();
    ResponseHelper.validateResponse(response);
    return new PageImpl<>(
        response.getContent().stream().map(ResponseHelper::toUiValidationRestrictedKeywordsWebResponse)
            .collect(Collectors.toList()));
  }

  @Override
  public Page<RestrictedKeywordsListingWebResponse> getRestrictedKeywordForListing(String keyword, int page, int size) {
    GdnPreconditions.checkArgument(Objects.nonNull(keyword), ErrorMessages.KEYWORD_MUST_NOT_BE_NULL);
    GdnRestListResponse<RestrictedKeywordsListingResponse> response =
        pcbFeign.getRestrictedKeywordForListing(keyword, page, size);
    ResponseHelper.validateResponse(response);
    return new PageImpl<>(response.getContent().stream().map(ResponseHelper::toRestrictedKeywordsListingWebResponse)
        .collect(Collectors.toList()), PageRequest.of(page, size), response.getPageMetaData().getTotalRecords());
  }

}
