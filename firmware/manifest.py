import makemehappy.manifest as m
import makemehappy.git as git
import makemehappy.pathlike as p

sd = p.SourceDirectory(git.toplevel())

def chipRemote(prefix, log, vcs):
    import makemehappy.manifest as m
    import makemehappy.pathlike as p
    def _generate(instance):
        (kind, board, app, tc, cfg) = instance.split('/')
        bd = p.BuildDirectory(prefix, instance, log)
        return (m.zephyr(bd, 'chip-remote')
                .filter(m.remove(r'/autoconf\.h$'))
                .transform(m.withVersion(vcs))
                .transform(m.withDashString(cfg))
                .destination(board))
    return _generate

chipRemoteInstances = map(chipRemote(build_prefix, logging, sd.vcs),
                          system_instances)

m.manifest(list(chipRemoteInstances),
           m.file('README')            .rename('README.firmware'),
           m.file('README',  root = sd).rename('README.chip-remote'),
           m.file('LICENCE', root = sd))

def adjustInstall(infile, outfile):
    import re
    if re.match(r'^.*\.exe$', str(outfile)):
        outfile.chmod(0o755)

m.manifest.subdir(sd.vcs.version())
m.manifest.withInstallCallback(adjustInstall)
