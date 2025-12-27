package com.gdn.partners.pcu.external.service.impl.helper;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class FileValidatorTest {

  @Test
  void testCheckFileByType_JPEG() {
    byte[] jpegHeader = new byte[] {(byte) 0xFF, (byte) 0xD8};
    assertTrue(FileValidator.checkFileByType(jpegHeader, FileValidator.JPEG), "Should match JPEG type");
  }

  @Test
  void testCheckFileByType_PNG() {
    byte[] pngHeader = new byte[] {(byte) 0x89, 0x50};
    assertTrue(FileValidator.checkFileByType(pngHeader, FileValidator.PNG), "Should match PNG type");
  }

  @Test
  void testCheckFileByType_XLSX() {
    byte[] xlsxHeader = new byte[] {0x50, 0x4B, 0x03, 0x04, 0x14, 0x00};
    assertTrue(FileValidator.checkFileByType(xlsxHeader, FileValidator.XLSX), "Should match XLSX type");
  }

  @Test
  void testCheckFileByType_InvalidType() {
    byte[] invalidHeader = new byte[] {0x12, 0x34};
    assertFalse(FileValidator.checkFileByType(invalidHeader, FileValidator.JPEG), "Should not match any type");
  }

  @Test
  void testCheckFileByType_EmptyBytes() {
    byte[] emptyBytes = new byte[] {};
    assertFalse(FileValidator.checkFileByType(emptyBytes, FileValidator.PNG), "Should not match with empty bytes");
  }

  @Test
  void testCheckFileByType_NullBytes() {
    assertThrows(IllegalArgumentException.class, () -> FileValidator.checkFileByType(null, FileValidator.PDF));
  }

  @Test
  void testToHexFromByte() {
    byte input = (byte) 0xAB;
    String expectedHex = "ab";
    assertEquals(expectedHex, FileValidator.toHexFromByte(input), "Should convert byte to correct hex string");
  }

  @Test
  void testToHexFromBytes() {
    byte[] input = new byte[] {(byte) 0xFF, (byte) 0xD8, (byte) 0xFF};
    String expectedHex = "ffd8ff";
    assertEquals(expectedHex, FileValidator.toHexFromBytes(input), "Should convert bytes to correct hex string");
  }

  @Test
  void testToHexFromBytes_EmptyArray() {
    byte[] emptyArray = new byte[] {};
    assertEquals("", FileValidator.toHexFromBytes(emptyArray), "Should return empty string for empty byte array");
  }

  @Test
  void testToHexFromBytes_NullArray() {
    assertEquals("", FileValidator.toHexFromBytes(null), "Should return empty string for null byte array");
  }
}
