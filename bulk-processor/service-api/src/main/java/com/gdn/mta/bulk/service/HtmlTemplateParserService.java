package com.gdn.mta.bulk.service;

import java.util.Map;

public interface HtmlTemplateParserService {
  /**
   * Generates static HTML by parsing VTL template using {@code variables}.
   *
   * @param bulkProcessCode
   * @param template
   * @param variables
   * @return static HTML code
   */
  String parseVelocityTemplateToHtml(String bulkProcessCode, String template,
      Map<String, Object> variables);
}
