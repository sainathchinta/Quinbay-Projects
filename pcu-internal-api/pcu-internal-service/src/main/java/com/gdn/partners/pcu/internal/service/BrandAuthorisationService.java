package com.gdn.partners.pcu.internal.service;

import java.util.List;

import com.gdn.partners.pcu.internal.client.model.request.BrandAuthDeleteRequest;
import org.springframework.data.domain.Page;
import org.springframework.web.multipart.MultipartFile;

import com.gdn.partners.pcu.internal.web.model.request.BrandAuthWebRequest;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.partners.pcu.internal.web.model.response.BrandAuthCreateWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.BrandAuthFilterWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.BrandAuthorisationDetailWebResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthHistoryRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthHistoryResponse;
import com.gdn.x.productcategorybase.dto.request.BrandAuthUpdateRequest;

public interface BrandAuthorisationService {

  /**
   * Returns brand Authorisation details by brand code and Seller Code
   *
   * @param brandCode
   * @param sellerCode
   * @param storeId
   * @return
   */
  BrandAuthorisationDetailWebResponse getBrandAuthDetailBySellerCodeAndBrandCode(String storeId,
    String sellerCode, String brandCode) throws Exception;

  /**
   *
   * @param storeId
   * @param username
   * @param requestId
   * @param request
   * @return
   */
  BrandAuthCreateWebResponse create(String storeId, String username, String requestId,
    BrandAuthCreateRequest request);

  /**
   * Brand auth update
   *
   * @param storeId
   * @param username
   * @param requestId
   * @param request
   * @return
   */
  void update(String storeId, String username, String requestId,
      BrandAuthUpdateRequest request);

  /**
   *
   * @param storeId
   * @param username
   * @param brandAuthDeleteRequestList
   * @throws Exception
   */
  void delete(String storeId, String username, List<BrandAuthDeleteRequest> brandAuthDeleteRequestList) throws Exception;

  /**
   * Get brand auths based on filter
   *
   * @param brandAuthWebRequest
   */
  Page<BrandAuthFilterWebResponse> getAuthorisations(BrandAuthWebRequest brandAuthWebRequest, int page, int size);

  /**
   * Fetch brand history
   *
   * @param brandAuthHistoryRequest
   * @param page
   * @param size
   * @param storeId
   * @param requestId
   * @param username
   * @return
   */
  GdnRestListResponse<BrandAuthHistoryResponse> getBrandAuthHistory(String storeId,
    String requestId, String username, BrandAuthHistoryRequest brandAuthHistoryRequest, int page,
    int size);

  /**
   * @param fileName      file name of the doc
   * @param brandCode     brand code of the brand doc
   * @param sellerCode    seller code of the brand doc
   * @param multipartFile file
   */
  void uploadBrandAuthDoc(String fileName, String brandCode, String sellerCode,
    byte[] multipartFile) throws Exception;

  /**
   * Bulk upload Brand Authorization
   * @param request
   * @param processType
   * @param requestId
   * @param storeId
   * @param username
   */
  void saveBulkBrandAuthFile(MultipartFile request, String processType, String requestId, String storeId,
      String username) throws Exception;

  /**
   *
   * @param username
   * @param brandAuthWebRequest
   */
  void brandAuthDownloadAll(String username, BrandAuthWebRequest brandAuthWebRequest);
}