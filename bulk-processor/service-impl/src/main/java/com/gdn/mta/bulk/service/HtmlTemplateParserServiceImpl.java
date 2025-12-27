package com.gdn.mta.bulk.service;

import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.Velocity;
import org.apache.velocity.tools.generic.ConversionTool;
import org.apache.velocity.tools.generic.DateTool;
import org.apache.velocity.tools.generic.NumberTool;
import org.springframework.stereotype.Service;

import java.io.StringWriter;
import java.io.Writer;
import java.util.Locale;
import java.util.Map;
import java.util.TimeZone;

@Service
public class HtmlTemplateParserServiceImpl implements HtmlTemplateParserService {
  @Override
  public String parseVelocityTemplateToHtml(String bulkProcessCode, String template,
      Map<String, Object> variables) {
    VelocityContext context = new VelocityContext(variables);
    context.put("numberTool", new NumberTool());
    context.put("dateTool", new DateTool());
    context.put("conversionTool", new ConversionTool());
    context.put("locale", new Locale("in", "ID"));
    context.put("timezone", TimeZone.getTimeZone("GMT+7"));

    Writer writer = new StringWriter();
    Velocity.evaluate(context, writer, bulkProcessCode, template);
    return writer.toString();
  }
}
