package uk.co.nstauthority.scap.file;

import org.springframework.boot.context.properties.ConfigurationProperties;

@ConfigurationProperties(prefix = "file-upload")
public record FileUploadConfig(Integer maxAllowedSize,
                        String allowedExtensions,
                        String filenameDisallowedCharactersRegex) {

}