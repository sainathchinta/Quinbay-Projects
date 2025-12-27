package com.gdn.partners.pcu.external.service.impl;

import jakarta.servlet.http.HttpServletResponse;

import org.apache.poi.ss.usermodel.Workbook;
import org.springframework.stereotype.Service;

import com.gdn.partners.pcu.external.service.BrandAuthService;
import com.gdn.partners.pcu.external.service.impl.helper.ExcelTemplateUtil;
import com.gdn.partners.pcu.external.web.model.request.BulkBrandDataRequest;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class BrandAuthServiceImpl implements BrandAuthService {

  private static final String FILE_BULK_SELECTED_BRAND_DOWNLOAD = "bulk-selected-brand-template";

  @Override
  public void selectedBrandBulkDownload(HttpServletResponse servletResponse, BulkBrandDataRequest bulkBrandDataRequest)
      throws Exception {
    Workbook workbook = ExcelTemplateUtil.generateWorkbookTemplateSelectedBrandBulk(bulkBrandDataRequest);
    ExcelTemplateUtil.generateFileTemplate(FILE_BULK_SELECTED_BRAND_DOWNLOAD, workbook, servletResponse);
  }
}

