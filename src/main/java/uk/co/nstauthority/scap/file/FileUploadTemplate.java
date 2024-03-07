package uk.co.nstauthority.scap.file;

public record FileUploadTemplate(
    String downloadUrl,
    String uploadUrl,
    String deleteUrl,
    String maxAllowedSize,
    String allowedExtensions
) {
}