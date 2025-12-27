package com.gdn.partners.pcu.master.service;

import com.gdn.partners.pcu.master.web.model.request.RestrictedKeywordsSearchWebRequest;
import com.gdn.partners.pcu.master.web.model.response.RestrictedKeywordsWebResponse;
import org.springframework.data.domain.Page;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.pcu.master.web.model.response.RestrictedKeywordHistoryWebResponse;
import com.gdn.partners.pcu.master.web.model.response.RestrictedKeywordsListingWebResponse;
import com.gdn.x.productcategorybase.dto.request.RestrictedKeywordsUpdateRequest;
import com.gdn.partners.pcu.master.web.model.response.UiValidationRestrictedKeywordsWebResponse;

public interface RestrictedKeywordService {

  /**
   * Getting Restricted Keyword History
   *
   * @param keyword
   * @param page
   * @param size
   * @return
   */
  Page<RestrictedKeywordHistoryWebResponse> getRestrictedKeywordHistory(String keyword, int page, int size);

  /**
   * Getting Restricted Keywords
   *
   * @param request
   * @param page
   * @param size
   * @return
   */
  Page<RestrictedKeywordsWebResponse> getRestrictedKeywords(RestrictedKeywordsSearchWebRequest request, int page, int size);


  /**
   * Add/delete/Update restricted keyword
   * @param restrictedKeywordsUpdateRequest
   * @return
   */
  GdnBaseRestResponse upsertRestrictedKeyword(RestrictedKeywordsUpdateRequest restrictedKeywordsUpdateRequest);

  /**
   * Getting Ui Validation Restricted Keywords
   *
   * @return
   */
  Page<UiValidationRestrictedKeywordsWebResponse> getUiValidationRestrictedKeywords();

  /**
   * Getting Restricted Keyword For Listing
   *
   * @param keyword
   * @param page
   * @param size
   * @return
   */
  Page<RestrictedKeywordsListingWebResponse> getRestrictedKeywordForListing(String keyword, int page, int size);
}
