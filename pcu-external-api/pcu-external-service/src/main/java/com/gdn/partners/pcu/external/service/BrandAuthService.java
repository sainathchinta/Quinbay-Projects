package com.gdn.partners.pcu.external.service;

import jakarta.servlet.http.HttpServletResponse;

import com.gdn.partners.pcu.external.web.model.request.BulkBrandDataRequest;

public interface BrandAuthService {
  void selectedBrandBulkDownload(HttpServletResponse servletResponse, BulkBrandDataRequest bulkBrandDataRequest) throws Exception;
}
