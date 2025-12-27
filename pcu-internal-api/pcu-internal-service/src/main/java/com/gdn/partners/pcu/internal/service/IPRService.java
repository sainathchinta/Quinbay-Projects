package com.gdn.partners.pcu.internal.service;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.pcu.internal.client.model.request.IPRProductListRequest;
import com.gdn.partners.pcu.internal.client.model.request.IprActionRequest;
import com.gdn.partners.pcu.internal.client.model.response.IPRProductDetailResponse;
import com.gdn.partners.pcu.internal.client.model.request.IPRUpdateAssigneeRequest;
import com.gdn.partners.pcu.internal.client.model.response.IPRProductHistoryResponse;
import com.gdn.partners.pcu.internal.client.model.response.IPRProductListResponse;
import com.gdn.partners.pcu.internal.client.model.response.IprSuspensionInProgressWebResponse;
import com.gdn.partners.pcu.internal.web.model.request.IPRProductsDownloadWebRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.MapResponse;
import com.gdn.partners.pcu.internal.client.model.response.IprSuspensionInProgressResponse;
import org.springframework.data.domain.Page;

import java.util.List;

public interface IPRService {

  /**
   * to fetch list of products for IPR
   *
   * @param page
   * @param size
   * @param iprProductListRequest
   * @return
   */
  Page<IPRProductListResponse> getIPRProductList(int page, int size,
      IPRProductListRequest iprProductListRequest);

  /**
   * Get IPR reviewers list
   * @return List of reviewers
   */
  List<String> getIPRReviewers() throws Exception;

  /**
   * Fetch IPR Product Detail
   *
   * @param productSku
   * @return
   */
  IPRProductDetailResponse getIPRProductDetailByProductSku(String productSku);

  /**
   * update assignee for product
   *
   * @param iprUpdateAssigneeRequest
   * @throws Exception
   */
  void updateAssignee(IPRUpdateAssigneeRequest iprUpdateAssigneeRequest) throws Exception;

  /**
   * get primary filter counts
   *
   * @return MapResponse
   */
  MapResponse getPrimaryFilterCounts();

  /**
   * to fetch ipr products which are in suspension in progress state (evidence requested,
   * evidence submitted) for a particular seller
   *
   * @param page                int
   * @param size                int
   * @param businessPartnerCode String
   * @param sortOrder
   * @return list of IprSuspensionInProgressWebResponse
   */
  Page<IprSuspensionInProgressWebResponse> getSuspensionInProcessProducts(int page, int size,
      String businessPartnerCode, String sortOrder);

  /**
   * to perform actions - release, whitelist, suspend, request evidence on ipr products
   *
   * @param iprActionRequest IprActionRequest
   * @return GdnBaseRestResponse
   */
  GdnBaseRestResponse performIprAction(IprActionRequest iprActionRequest);

  /**
   * Fetch history for IPR product
   *
   * @param page
   * @param size
   * @param productSku
   * @return
   */
  Page<IPRProductHistoryResponse> fetchIprProductHistory(int page, int size, String productSku);

  /**
   * Mass Download IPR Products
   *
   * @param username non null username
   * @param request non null request
   */
  void downloadIPRProducts(String username, IPRProductsDownloadWebRequest request);

}
