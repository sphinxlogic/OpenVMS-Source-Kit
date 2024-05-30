- All archives with version 7.x contain programs which will run on both
VMS V7.2 and VMS V7.3, unless the cover letter of the package says otherwise.

- All PCSI packages contained within a self-extracting archive have their
extension PCSI-DCX_VAXEXE changed to EXE. This was done so that some essential
packages (e.g. CC064 (C compiler) and UCX053 (TCP/IP support)) could be stored
on an ISO9660 filesystem.

- BACKUP savesets must have their attributes reset after download. A DCL script
to do this can be downloaded from

	http://h71000.www7.hp.com/freeware/reset_backup_saveset_file_attributes.zip

or

	http://decuslib.com/decus/freewarev70/000TOOLS/reset_backup_saveset_file_attributes.com

Alternatively, you may use
$ SET FILE/ATTRIBUTES=(RFM:FIX,MRS:32256,LRL:32256,RAT:NONE) file.bck

DEC OpenVMS 7 Kits (2003).7z