package com.gdn.partners.bulk.util;

import java.io.File;
import java.io.IOException;

import com.gdn.common.exception.ApplicationException;

/**
 * @author agie.falah
 *
 * Used to validate product's image that uploaded through bulk process, like MTA-API and excel upload
 */
public interface ProductImageValidator {
  public void validateImages(File imageFile);
}
